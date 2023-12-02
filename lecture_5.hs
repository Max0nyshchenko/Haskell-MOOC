data Color = Red | Green | Blue

rgb :: Color -> [Double]
rgb Red = [1,0,0]
rgb Green = [0,1,0]
rgb Blue = [0,0,1]

data Report = ConstructReport Int String String

reportContents :: Report -> String
reportContents (ConstructReport id title content) = content
setReportContents :: String -> Report -> Report
setReportContents contents (ConstructReport id title _contents) = ConstructReport id title contents

data Card = Joker | Heart Int | Club Int | Spade Int | Diamond Int
  deriving Show

data Described a = Describe a String

getValue:: Described a -> a
getValue (Describe x _) = x

getDescription :: Described a -> String
getDescription (Describe _ desc) = desc

data IntList = Empty | Node Int IntList
  deriving Show

ihead :: IntList -> Int
ihead (Node i _) = i

itail :: IntList -> IntList
itail (Node _ t) = t

ilength :: IntList -> Int
ilength Empty = 0
ilength (Node _ t) = 1 + ilength t

data List a = EmptyList | ListNode a (List a)
  deriving Show

lhead :: List a -> a
lhead (ListNode h _) = h

data Tree a = TreeNode a (Tree a) (Tree a) | EmptyTree

exampleTree :: Tree Int
exampleTree = (TreeNode 0 (TreeNode 1 (TreeNode 2 EmptyTree EmptyTree) 
                              (TreeNode 3 EmptyTree EmptyTree)) 
                      (TreeNode 4 EmptyTree EmptyTree))

treeHeight :: Tree a -> Int
treeHeight EmptyTree = 0
treeHeight (TreeNode _ l r) = 1 + max (treeHeight l) (treeHeight r)

data Person = MkPerson String Int String String String deriving Show

people :: [Person]
people = [MkPerson "Jane Doe" 21 "Houston" "Texas" "Engineer"]

query :: [Person] -> [Person]
query [] = []
query ((MkPerson name age town state profession):xs)
  | state == "Finland" && profession == "Engineer" = (MkPerson name age town state profession):query xs
  | otherwise = query xs

data RecordPerson = MkRecordPerson {name::String, age::Int, town::String, state::String, profession::String}
  deriving Show

recordPerson :: RecordPerson
recordPerson = MkRecordPerson {name = "Jane Doe", age = 21, town = "Houston", state = "Texas", profession = "Engineer"}

recordPersonProfession :: String
recordPersonProfession = profession recordPerson

queryRecordPerson :: [RecordPerson] -> [RecordPerson]
queryRecordPerson [] = []
queryRecordPerson (x:xs)
  | state x == "Finland" && profession x == "Engineer" = x:queryRecordPerson xs
  | otherwise = queryRecordPerson xs
