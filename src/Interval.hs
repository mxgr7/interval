{-# LANGUAGE IncoherentInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
module Interval where

import           Control.Lens
import qualified Data.List.NonEmpty as NE
import qualified Data.Map as M
import           Yahp hiding (complement)

-- | inclusive of bounds, non-empty
data Interval a = UnsafeInterval { unsafeLower :: a
                                 , unsafeUpper :: a
                                 }
  deriving (Eq)

type InfiniteInterval a = Interval (Extended a)

data Extended a = NegativeInfinity | PositiveInfinity | Finite !a
  deriving (Show, Eq, Functor)

makeLensesWith (lensField .~ mappingNamer (\f -> [f <> "_"]) $ lensRules) ''Interval
makePrisms ''Extended

fromExtended :: Extended a -> Maybe a
fromExtended = \case { Finite x -> Just x; _ -> Nothing }
{-# INLINABLE fromExtended #-}

instance Bounded (Extended a) where
  minBound = NegativeInfinity
  maxBound = PositiveInfinity

instance Show a => Show (Interval a) where
  show = show . toBounds

unsafeI = on UnsafeInterval Finite
{-# INLINABLE unsafeI #-}

iLower :: Interval a -> a
iLower = unsafeLower
{-# INLINEABLE iLower #-}

iUpper :: Interval a -> a
iUpper = unsafeUpper
{-# INLINEABLE  iUpper #-}


instance Ord a => Ord (Extended a) where
  compare NegativeInfinity      NegativeInfinity        = EQ
  compare PositiveInfinity      PositiveInfinity        = EQ
  compare _                     PositiveInfinity        = LT
  compare PositiveInfinity      _                       = GT
  compare NegativeInfinity      _                       = LT 
  compare _                     NegativeInfinity        = GT 
  compare (Finite a)            (Finite b)              = compare a b 
  {-# INLINABLE compare #-}

toBounds :: Interval c -> (c, c)
toBounds = iLower &&& iUpper
{-# INLINABLE toBounds #-}

toI :: Interval a -> InfiniteInterval a
toI i = unsafeI (iLower i) $ iUpper i
{-# INLINABLE toI #-}

fromI :: Bounded a => InfiniteInterval a -> Interval a
fromI i = on UnsafeInterval g (iLower i) $ iUpper i
  where g = \case NegativeInfinity      -> minBound
                  PositiveInfinity      -> maxBound
                  Finite x              -> x
{-# INLINABLE fromI #-}

unbounded :: Bounded a => Interval a
unbounded = UnsafeInterval minBound maxBound
{-# INLINABLE unbounded #-}

singleton :: a -> Interval a
singleton x = UnsafeInterval x x
{-# INLINE singleton #-}

hull :: Ord a => a -> a -> Interval a
hull x y = UnsafeInterval (min x y) (max x y)
{-# INLINABLE hull #-}

hullOfList :: Ord a => NonEmpty a -> Interval a
hullOfList l = UnsafeInterval (minimum l) $ maximum l
{-# INLINABLE hullOfList #-}

hullOfIntervals :: Ord b => NonEmpty (Interval b) -> Interval b
hullOfIntervals = uncurry UnsafeInterval . (minimum *** maximum) . NE.unzip . fmap toBounds
{-# INLINABLE hullOfIntervals #-}

fromLowerBound :: Bounded a => a -> Interval a
fromLowerBound x = UnsafeInterval x maxBound
{-# INLINABLE fromLowerBound #-}

fromUpperBound :: Bounded a => a -> Interval a
fromUpperBound x = UnsafeInterval minBound x
{-# INLINABLE fromUpperBound #-}

fromBounds :: Ord a => a -> a -> Maybe (Interval a)
fromBounds a b = case compare a b of GT -> Nothing
                                     _  -> Just $ UnsafeInterval a b
{-# INLINABLE fromBounds #-}

fromBoundsI :: Ord a => a -> a -> Maybe (InfiniteInterval a)
fromBoundsI = fmap3 toI fromBounds
{-# INLINABLE fromBoundsI #-}


intersection :: Ord a => Interval a -> Interval a -> Maybe (Interval a)
intersection = (\(l1, u1) (l2, u2) -> fromBounds (max l1 l2) (min u1 u2)) `on` toBounds
{-# INLINABLE intersection #-}

intersectionSome :: (Bounded a, Foldable f, Ord a) => f (Interval a) -> Maybe (Interval a)
intersectionSome = intersectionMany . (unbounded :|) . toList
{-# INLINABLE intersectionSome #-}

-- | and [] = True
intersectionMany :: (Ord a) => NonEmpty (Interval a) -> Maybe (Interval a)
intersectionMany = foldl1' (\a b -> join $ liftA2 intersection a b) . fmap Just
{-# INLINABLE intersectionMany #-}

class SuccPred a where
  succ' :: a -> Maybe a
  pred' :: a -> Maybe a

instance (Ord a, Eq a, Bounded a, Enum a) => SuccPred a where
  succ' x = bool (Just $ succ x) Nothing $ x == maxBound
  pred' x = bool (Just $ pred x) Nothing $ x == minBound
  {-# INLINABLE succ' #-}
  {-# INLINABLE pred' #-}

instance {-# OVERLAPS #-} (Enum a, Ord a) => SuccPred (Extended a) where
  succ' = fmap2 (Finite . succ) fromExtended
  pred' = fmap2 (Finite . pred) fromExtended
  {-# INLINABLE succ' #-}
  {-# INLINABLE pred' #-}

complement :: (Ord a, Bounded a, SuccPred a) => Interval a -> [Interval a]
complement i = catMaybes [ fromUpperBound <$> pred' (iLower i) , fromLowerBound <$> succ' (iUpper i)]
{-# INLINABLE complement #-}

-- | a \ b = a ∩ complement b
difference :: (Ord a, Bounded a, SuccPred a) => Interval a -> Interval a -> [Interval a]
difference r1 = mapMaybe (intersection r1) . complement
{-# INLINEABLE difference #-}

enumInterval :: Enum a => Interval a -> [a]
enumInterval = uncurry enumFromTo . toBounds
{-# INLINEABLE enumInterval #-}

-- | this is unsafe. for example maxBound + 1 = (minBound ::Int)
unsafeTranslate :: Num a => a -> Interval a -> Interval a
unsafeTranslate v i = i & unsafeLower_ %~ (v+) & unsafeUpper_ %~ (v+)
{-# INLINABLE unsafeTranslate #-}

translateI :: Num a => a -> InfiniteInterval a -> InfiniteInterval a
translateI v i = i & unsafeLower_ . _Finite %~ (v+) & unsafeUpper_ . _Finite %~ (v+)
{-# INLINABLE translateI #-}

containsI :: Ord a => InfiniteInterval a -> a -> Bool
containsI r x' = let x = Finite x' in iLower r <= x && x <= iUpper r
{-# INLINEABLE containsI #-}

contains :: Ord a => Interval a -> a -> Bool
contains r x = iLower r <= x && x <= iUpper r
{-# INLINEABLE contains #-}

closest :: Ord a => a -> Interval a -> a
closest x r | x < lower      = lower
            | x > upper      = upper
            | True           = x
  where (lower,upper) = toBounds r
{-# INLINEABLE closest #-}



-- * step functions

newtype StepFunction x y = UnsafeStepFunction { fromUnsafeStepFunction :: M.Map x (Maybe y) }
  deriving (Eq, Show) via M.Map x (Maybe y)

fromStepFunction :: StepFunction x y -> Map x (Maybe y)
fromStepFunction = fromUnsafeStepFunction
{-# INLINEABLE fromStepFunction #-}

nowhere :: StepFunction x y
nowhere = UnsafeStepFunction M.empty
{-# INLINE nowhere #-}

everywhere :: Bounded x => y -> StepFunction x y
everywhere = UnsafeStepFunction . M.singleton maxBound . Just
{-# INLINABLE everywhere #-}

toStepFunction :: forall f y x . (Ord x, Bounded x, Foldable f, Enum x, Show x)
               => f (Interval x, y) -> Either Text (StepFunction x y)
toStepFunction = fmap (UnsafeStepFunction . M.fromDistinctAscList) . g minBound .
  sortBy (comparing $ fst . fst) . fmap (first toBounds) . toList
  where
    g :: x -> [((x,x), y)] -> Either Text [(x, Maybe y)]
    g _ []                                 = Right []
    g prevUp (((l,u),v):r) | l == minBound      = rest
                           | True               = f $ pred l
      where f beforeL   | prevUp < beforeL      = ((beforeL,Nothing) :) <$> rest
                        | prevUp == beforeL     = rest
                        | True                  = Left $ toS err
            rest = ((u, Just v) :) <$> g u r
            err = "Overlapping intervals in step function: " <> show prevUp <> " >= " <> show l
{-# INLINEABLE toStepFunction #-}

toStepFunction' :: (Ord x, Bounded x, Foldable f, Enum x, Show x, Functor f) =>
                  f (InfiniteInterval x, y) -> Either Text (StepFunction x y)
toStepFunction' = toStepFunction . fmap (first fromI)
{-# INLINEABLE toStepFunction' #-}

(!) :: Ord x => StepFunction x y -> x -> Maybe y
st ! x = snd =<< M.lookupGE x (fromStepFunction st)
{-# INLINEABLE (!) #-}
