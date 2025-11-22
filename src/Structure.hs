module Structure
  ( PrefixTreeDict,
    emptyDict,
    insertDict,
    deleteDict,
    lookupDict,
    memberDict,
    filterDict,
    foldlDict,
    foldrDict,
    mapDict,
    mergeDict,
  )
where

import Data.Maybe (isJust)

data PrefixTreeDict v
  = Node
      { value :: !(Maybe v),
        children :: !(Children v)
      }
  deriving (Show)

data Children v = ChildrenNil | ChildrenCons !Char !(PrefixTreeDict v) !(Children v)
  deriving (Show)

emptyDict :: PrefixTreeDict v
emptyDict = Node {value = Nothing, children = ChildrenNil}

insertDict :: String -> v -> PrefixTreeDict v -> PrefixTreeDict v
insertDict [] v node = node {value = Just v}
insertDict (c : cs) v node =
  let children' = insertChild c cs v (children node)
   in node {children = children'}

deleteDict :: String -> PrefixTreeDict v -> PrefixTreeDict v
deleteDict [] node = node {value = Nothing}
deleteDict (c : cs) node =
  let children' = deleteChild c cs (children node)
   in node {children = children'}

lookupDict :: String -> PrefixTreeDict v -> Maybe v
lookupDict [] node = value node
lookupDict (c : cs) node = lookupChild c cs (children node)

memberDict :: String -> PrefixTreeDict v -> Bool
memberDict key dict = isJust (lookupDict key dict)

filterDict :: (String -> v -> Bool) -> PrefixTreeDict v -> PrefixTreeDict v
filterDict p = filterNode p ""

foldlDict :: (b -> String -> v -> b) -> b -> PrefixTreeDict v -> b
foldlDict f z = foldlNode f z ""

foldrDict :: (String -> v -> b -> b) -> b -> PrefixTreeDict v -> b
foldrDict f z = foldrNode f z ""

mapDict :: (v -> w) -> PrefixTreeDict v -> PrefixTreeDict w
mapDict = mapNode

mergeDict :: PrefixTreeDict v -> PrefixTreeDict v -> PrefixTreeDict v
mergeDict lhs rhs =
  let insertFn acc key val = insertDict key val acc
   in foldlDict insertFn lhs rhs

insertChild :: Char -> String -> v -> Children v -> Children v
insertChild c rest v ChildrenNil =
  ChildrenCons c (insertDict rest v emptyDict) ChildrenNil
insertChild c rest v (ChildrenCons ch node siblings)
  | c == ch = ChildrenCons ch (insertDict rest v node) siblings
  | c < ch = ChildrenCons c (insertDict rest v emptyDict) (ChildrenCons ch node siblings)
  | otherwise = ChildrenCons ch node (insertChild c rest v siblings)

deleteChild :: Char -> String -> Children v -> Children v
deleteChild _ _ ChildrenNil = ChildrenNil
deleteChild c rest (ChildrenCons ch node siblings)
  | c == ch =
      let node' = deleteDict rest node
       in if isEmptyNode node'
            then siblings
            else ChildrenCons ch node' siblings
  | otherwise = ChildrenCons ch node (deleteChild c rest siblings)

lookupChild :: Char -> String -> Children v -> Maybe v
lookupChild _ _ ChildrenNil = Nothing
lookupChild c rest (ChildrenCons ch node siblings)
  | c == ch = lookupDict rest node
  | otherwise = lookupChild c rest siblings

isEmptyNode :: PrefixTreeDict v -> Bool
isEmptyNode node = isNothing (value node) && isChildrenEmpty (children node)
  where
    isNothing Nothing = True
    isNothing (Just _) = False

isChildrenEmpty :: Children v -> Bool
isChildrenEmpty ChildrenNil = True
isChildrenEmpty _ = False

filterNode :: (String -> v -> Bool) -> String -> PrefixTreeDict v -> PrefixTreeDict v
filterNode p prefix node =
  let val' = case value node of
        Just v -> if p prefix v then Just v else Nothing
        Nothing -> Nothing
      children' = filterChildren p prefix (children node)
   in Node {value = val', children = children'}

filterChildren :: (String -> v -> Bool) -> String -> Children v -> Children v
filterChildren _ _ ChildrenNil = ChildrenNil
filterChildren p prefix (ChildrenCons ch node siblings) =
  let node' = filterNode p (prefix ++ [ch]) node
      siblings' = filterChildren p prefix siblings
   in if isEmptyNode node'
        then siblings'
        else ChildrenCons ch node' siblings'

foldlNode :: (b -> String -> v -> b) -> b -> String -> PrefixTreeDict v -> b
foldlNode f acc prefix node =
  let acc' = case value node of
        Just v -> f acc prefix v
        Nothing -> acc
   in foldlChildren f acc' prefix (children node)

foldlChildren :: (b -> String -> v -> b) -> b -> String -> Children v -> b
foldlChildren _ acc _ ChildrenNil = acc
foldlChildren f acc prefix (ChildrenCons ch node siblings) =
  let acc' = foldlNode f acc (prefix ++ [ch]) node
   in foldlChildren f acc' prefix siblings

foldrNode :: (String -> v -> b -> b) -> b -> String -> PrefixTreeDict v -> b
foldrNode f acc prefix node =
  let acc' = foldrChildren f acc prefix (children node)
   in case value node of
        Just v -> f prefix v acc'
        Nothing -> acc'

foldrChildren :: (String -> v -> b -> b) -> b -> String -> Children v -> b
foldrChildren _ acc _ ChildrenNil = acc
foldrChildren f acc prefix (ChildrenCons ch node siblings) =
  let acc' = foldrChildren f acc prefix siblings
   in foldrNode f acc' (prefix ++ [ch]) node

mapNode :: (v -> w) -> PrefixTreeDict v -> PrefixTreeDict w
mapNode f node =
  Node
    { value = fmap f (value node),
      children = mapChildren f (children node)
    }

mapChildren :: (v -> w) -> Children v -> Children w
mapChildren _ ChildrenNil = ChildrenNil
mapChildren f (ChildrenCons ch node siblings) =
  ChildrenCons ch (mapNode f node) (mapChildren f siblings)

instance Eq v => Eq (PrefixTreeDict v) where
  lhs == rhs =
    let lhsList = foldrDict (\k v acc -> (k, v) : acc) [] lhs
        rhsList = foldrDict (\k v acc -> (k, v) : acc) [] rhs
     in length lhsList == length rhsList
          && all (\(k, v) -> lookupDict k rhs == Just v) lhsList

instance Semigroup (PrefixTreeDict v) where
  (<>) = mergeDict

instance Monoid (PrefixTreeDict v) where
  mempty = emptyDict
  mappend = (<>)