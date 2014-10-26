data NestedList a = Elem a
                  | List [NestedList a]

flatten :: NestedList a -> [a] 
flatten x = case x of
  Elem x -> [x]
  List xs -> foldl (\a b -> a ++ flatten b) [] xs
