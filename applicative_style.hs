
--class Functor f => Applicative f where
--pure :: a -> f a
  --(<*>) :: f (a -> b) -> f a -> f b
import Control.Applicative

fmap0 :: Applicative f => a -> f a
fmap0 = pure

fmap1 :: Applicative f => (a -> b) -> f a -> f b
fmap1 g x = pure g <*> x

fmap2 :: Applicative f => (a -> b -> c) -> f a -> f b -> f c
fmap2 g x y = pure g <*> x <*> y

fmap3 :: Applicative f => (a -> b -> c -> d) -> f a -> f b -> f c -> f d
fmap3 g x y z = pure g <*> x <*> y <*> z
