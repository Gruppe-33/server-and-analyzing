module Zipper where

import Prelude

import Control.Comonad (class Comonad, class Extend, extend)
import Data.Function (applyN)
import Data.FunctorWithIndex (mapWithIndex)
import Data.Int (toNumber)
import Data.List (List(..), (:), reverse, uncons)
import Data.List.NonEmpty (NonEmptyList(..))
import Data.List.NonEmpty (toList, fromList) as NEL
import Data.Maybe (Maybe(..))
import Data.NonEmpty ((:|))

data Zipper a = Z (List a) a (List a)

instance functorZipper :: Functor Zipper where
  map f (Z left x right) = Z (f <$> left) (f x) (f <$> right)

-- instance applyZipper :: Apply Zipper where
--   apply = 

-- instance applicativeZipper :: Applicative Zipper where
--   pure x = Z Nil x Nil

instance extendZipper :: Extend Zipper where
  extend f zipper@(Z l x r) = Z
    (mapWithIndex funcl l)
    (f zipper)
    (mapWithIndex funcr r)
      where
        funcl idx _ = f (moveLeftN (idx + 1) zipper)
        funcr idx _ = f (moveRightN (idx + 1) zipper)

instance comonadZipper :: Comonad Zipper where
  extract (Z _ x _) = x

instance showZipper :: Show a => Show (Zipper a) where
  show (Z left x right)
     = "(Z " <> show left
    <> " "   <> show x
    <> " "   <> show right
    <> ")"


moveLeft :: forall a. Zipper a -> Zipper a
moveLeft (Z (l:ls) x rs) = Z ls l (x:rs)
moveLeft zipper@(Z Nil _ _) = zipper

moveLeftN :: forall a. Int -> Zipper a -> Zipper a
moveLeftN n = applyN moveLeft n

moveRight :: forall a. Zipper a -> Zipper a
moveRight (Z ls x (r:rs)) = Z (x:ls) r rs
moveRight zipper@(Z _ _ Nil) = zipper

moveRightN :: forall a. Int -> Zipper a -> Zipper a
moveRightN n = applyN moveRight n

fromNEL :: forall a. Int -> NonEmptyList a -> Zipper a
fromNEL n (NonEmptyList (x:|xs)) = moveRightN n (Z Nil x xs)

toNEL :: forall a. Zipper a -> NonEmptyList a
toNEL (Z l x r) = case uncons (reverse l) of
  Just { head, tail } -> NonEmptyList (head :| tail) <> NonEmptyList (x :| r)
  Nothing             -> NonEmptyList (x :| r)

fromList :: forall a. Int -> List a -> Maybe (Zipper a)
fromList n xs = NEL.fromList xs <#> fromNEL n

toList :: forall a. Zipper a -> List a
toList = toNEL >>> NEL.toList


test :: List Number
test = NonEmptyList (1:|2:3:4:3:2:1:Nil)
  # map toNumber
  # fromNEL 0
  # extend f
  # toList
    where
      f (Z Nil x (r:_))   = (x + r) / 2.0
      f (Z (l:_) x Nil)   = (l + x) / 4.0
      f (Z Nil x Nil)     = x
      f (Z (l:_) x (r:_)) = (l + x + r) / 3.0

a :: Zipper Int -> Int
a (Z Nil x (r:_)) = x + r
a (Z (l:_) x Nil) = l + x
a (Z (l:_) x (r:_)) = l + x + r
a (Z Nil x Nil) = x