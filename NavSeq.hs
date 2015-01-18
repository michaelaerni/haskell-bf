module NavSeq (
  NavSeq,
  createSeq,
  moveLeft,
  moveRight,
  appendLeft,
  appendRight,
  current,
  insert,
  isLeftEnd,
  isRightEnd
  )
where

  
data NavSeq a = LEnd [a] | Seq [a] a [a] | REnd [a] deriving Show

createSeq :: [a] -> NavSeq a
createSeq [] = error "A navigational sequence must contain at least one element"
createSeq (x:xs) = Seq [] x xs


moveRight :: NavSeq a -> NavSeq a
moveRight (REnd _) = error "Right end already reached"
moveRight (LEnd (r:rs)) = Seq [] r rs
moveRight (Seq ls x []) = REnd (x : ls)
moveRight (Seq ls x (r:rs)) = Seq (x : ls) r rs

moveLeft :: NavSeq a -> NavSeq a
moveLeft (LEnd _) = error "Left end already reached"
moveLeft (REnd (l:ls)) = Seq ls l []
moveLeft (Seq [] x rs) = LEnd (x : rs)
moveLeft (Seq (l:ls) x rs) = Seq ls l (x : rs)


appendLeft :: a -> NavSeq a -> NavSeq a
appendLeft x (LEnd rs) = Seq [] x rs
appendLeft x (Seq ls c rs) = Seq (ls ++ [x]) c rs

appendRight :: a -> NavSeq a -> NavSeq a
appendRight x (REnd ls) = Seq ls x []
appendRight x (Seq ls c rs) = Seq ls c (rs ++ [x])


current :: NavSeq a -> a
current (Seq _ x _) = x
current (LEnd _) = error "Currently at the left end of the sequence"
current (REnd _) = error "Currently at the right end of the sequence"

insert :: a -> NavSeq a -> NavSeq a
insert x (Seq ls _ rs) = Seq ls x rs
insert x seq@(LEnd _) = appendLeft x seq
insert x seq@(REnd _) = appendRight x seq


isLeftEnd :: NavSeq a -> Bool
isLeftEnd (LEnd _) = True
isLeftEnd _ = False

isRightEnd :: NavSeq a -> Bool
isRightEnd (REnd _) = True
isRightEnd _ = False
