module Structure.Internal
  ( PrefixTreeDict (..),
    emptyDict,
    insertDict,
    deleteDict,
    lookupDict,
    memberDict,
    filterDict,
    mergeDict,
    foldlWithKey,
    foldrWithKey,
  )
where

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (isJust)

data PrefixTreeDict v = Node
  { value :: !(Maybe v),
    children :: !(Map Char (PrefixTreeDict v))
  }
  deriving (Show)

emptyDict :: PrefixTreeDict v
emptyDict = Node {value = Nothing, children = Map.empty}

insertDict :: String -> v -> PrefixTreeDict v -> PrefixTreeDict v
insertDict [] v node = node {value = Just v}
insertDict (c : cs) v node =
  let child = Map.findWithDefault emptyDict c (children node)
      child' = insertDict cs v child
      children' = Map.insert c child' (children node)
   in node {children = children'}

deleteDict :: String -> PrefixTreeDict v -> PrefixTreeDict v
deleteDict [] node = node {value = Nothing}
deleteDict (c : cs) node =
  case Map.lookup c (children node) of
    Nothing -> node
    Just child ->
      let child' = deleteDict cs child
          children' =
            if isEmptyNode child'
              then Map.delete c (children node)
              else Map.insert c child' (children node)
       in node {children = children'}

lookupDict :: String -> PrefixTreeDict v -> Maybe v
lookupDict [] node = value node
lookupDict (c : cs) node = do
  child <- Map.lookup c (children node)
  lookupDict cs child

memberDict :: String -> PrefixTreeDict v -> Bool
memberDict key dict = isJust (lookupDict key dict)

filterDict :: (String -> v -> Bool) -> PrefixTreeDict v -> PrefixTreeDict v
filterDict p = filterNode p ""
  where
    filterNode :: (String -> v -> Bool) -> String -> PrefixTreeDict v -> PrefixTreeDict v
    filterNode predicate prefix node =
      let val' = case value node of
            Just v -> if predicate prefix v then Just v else Nothing
            Nothing -> Nothing
          children' = Map.mapWithKey (\c child -> filterNode predicate (prefix ++ [c]) child) (children node)
          children'' = Map.filter (not . isEmptyNode) children'
       in Node {value = val', children = children''}

mergeDict :: PrefixTreeDict v -> PrefixTreeDict v -> PrefixTreeDict v
mergeDict lhs rhs =
  let insertFn acc key val = insertDict key val acc
   in foldlWithKey insertFn lhs rhs

foldlWithKey :: (b -> String -> v -> b) -> b -> PrefixTreeDict v -> b
foldlWithKey = foldlWithPrefix ""
  where
    foldlWithPrefix :: String -> (b -> String -> v -> b) -> b -> PrefixTreeDict v -> b
    foldlWithPrefix prefix func z n =
      let z' = case value n of
            Just v -> func z prefix v
            Nothing -> z
       in Map.foldlWithKey (\acc' c child -> foldlWithPrefix (prefix ++ [c]) func acc' child) z' (children n)

foldrWithKey :: (String -> v -> b -> b) -> b -> PrefixTreeDict v -> b
foldrWithKey = foldrWithPrefix ""
  where
    foldrWithPrefix :: String -> (String -> v -> b -> b) -> b -> PrefixTreeDict v -> b
    foldrWithPrefix prefix func z n =
      let z' = Map.foldrWithKey (\c child acc' -> foldrWithPrefix (prefix ++ [c]) func acc' child) z (children n)
       in case value n of
            Just v -> func prefix v z'
            Nothing -> z'

isEmptyNode :: PrefixTreeDict v -> Bool
isEmptyNode node = isNothing (value node) && Map.null (children node)
  where
    isNothing Nothing = True
    isNothing (Just _) = False

instance Functor PrefixTreeDict where
  fmap f node =
    Node
      { value = fmap f (value node),
        children = fmap (fmap f) (children node)
      }

instance Foldable PrefixTreeDict where
  foldr = foldrValues
    where
      foldrValues :: (v -> b -> b) -> b -> PrefixTreeDict v -> b
      foldrValues func z n =
        let z' = foldr (flip (foldrValues func)) z (children n)
         in case value n of
              Just v -> func v z'
              Nothing -> z'

  foldl = foldlValues
    where
      foldlValues :: (b -> v -> b) -> b -> PrefixTreeDict v -> b
      foldlValues func z n =
        let z' = case value n of
              Just v -> func z v
              Nothing -> z
         in foldl (foldlValues func) z' (children n)

instance Eq v => Eq (PrefixTreeDict v) where
  lhs == rhs =
    let lhsList = foldrWithKey (\k v acc -> (k, v) : acc) [] lhs
        rhsList = foldrWithKey (\k v acc -> (k, v) : acc) [] rhs
     in length lhsList == length rhsList
          && all (\(k, v) -> lookupDict k rhs == Just v) lhsList

instance Semigroup (PrefixTreeDict v) where
  (<>) = mergeDict

instance Monoid (PrefixTreeDict v) where
  mempty = emptyDict
  mappend = (<>)