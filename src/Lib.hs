-- "Tower of Hamon" (c) by Ignacio Slater M.

-- "Tower of Hamon" is licensed under a
-- Creative Commons Attribution 4.0 International License.

-- You should have received a copy of the license along with this
-- work. If not, see <http://creativecommons.org/licenses/by/4.0/>.

-- v1.0-b.1
module Lib where

import           Control.Monad.State

someFunc :: IO ()
someFunc = putStrLn "someFunc"


type Disk = Int
data Peg = L | C | R deriving (Eq, Show)
type Conf = ([Disk], [Disk], [Disk])
type Move = (Peg, Peg)

type ConfState = State Conf

first :: Conf -> [Disk]
first (x, _, _) = x

second :: Conf -> [Disk]
second (_, x, _) = x

third :: Conf -> [Disk]
third (_, _, x) = x

{-|
Pushes a disk into a peg.
-}
push :: Disk -> Peg -> State Conf Conf
push disk peg = state (\conf -> (newConf conf, newConf conf))
 where
  newConf ogConf
    | peg == L  = (disk : first ogConf, second ogConf, third ogConf)
    | peg == C  = (first ogConf, disk : second ogConf, third ogConf)
    | otherwise = (first ogConf, disk : second ogConf, third ogConf)

c :: Conf
c = ([0], [1], [2])

pop :: Peg -> State Conf Disk
pop _ = undefined

{-| Moves a disk from one peg to another. -}
step :: Move -> State Conf Conf
step (src, dst) = do
  disk <- pop src
  push disk dst

{-| Computes the optimal sequence of moves to solve the Hanoi tower problem -}
optStrategy :: Int -> Move -> State Conf [(Move, Conf)]
optStrategy 1 move = do 
  conf <- step move
  return [(move, conf)]
optStrategy disks (src, dst) = do 
  mv1 <- optStrategy (disks - 1) (src, spare)
  mv2 <- optStrategy 1 (src, dst)
  mv3 <- optStrategy (disks - 1) (spare, dst)
  return $ mv1 ++ mv2 ++ mv3
 where
  spare =
    if src == L || dst == L then if src == C || dst == C then R else C else L

play :: Int -> Peg -> Peg -> IO ()
play = undefined
