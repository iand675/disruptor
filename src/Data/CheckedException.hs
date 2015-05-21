{-# LANGUAGE DataKinds #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE IncoherentInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverlappingInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
module Data.CheckedException
    ( Union (..)
    , (:<)
    , (:\)
    , (@>)
    , liftUnion
    , reUnion
    , restrict
    , typesExhausted
    , Checked
    , ok
    , report
    , onOk
    ) where

import Data.Dynamic

type Checked a = Either (Union a)

report :: (Applicative f, Typeable a, '[a] :< s) => a -> f (Checked s b)
report = pure . Left . liftUnion
{-# INLINE report #-}

ok :: Applicative f => a -> f (Checked s a)
ok = pure . Right
{-# INLINE ok #-}

onOk :: Monad m => Checked s a -> (a -> m b) -> m (Checked s b)
onOk (Right x) f = Right <$> f x
onOk (Left err) _ = return $ Left err

-- | The @Union@ type - the phantom parameter @s@ is a list of types
-- denoting what this @Union@ might contain.
-- The value contained is one of those types.
newtype Union (s :: [*]) = Union Dynamic

-- general note: try to keep from re-constructing Unions if an existing one
-- can just be type-coerced.

-- | Remove a type from anywhere in the list.
type family s :\ a where
    '[] :\ a = '[]
    (a ': s) :\ a = s :\ a
    (a' ': s) :\ a = a' ': (s :\ a)

-- | There exists a @s :< s'@ instance if every type in the list @s@
-- can be lifted to @s'@.
class (:<) (s :: [*]) (s' :: [*])
instance '[] :< s
instance (s :< s', Typeable a) => (a ': s) :< (a ': s')
instance (s :< s', '[a] :< s', Typeable a) => (a ': s) :< s'

-- | `restrict` in right-fixable style.
(@>) :: Typeable a => (a -> b) -> (Union (s :\ a) -> b) -> Union s -> b
r @> l = either l r . restrict
infixr 2 @>
{-# INLINE (@>) #-}

liftUnion :: (Typeable a, '[a] :< s) => a -> Union s
liftUnion = Union . toDyn
{-# INLINE liftUnion #-}

-- | Narrow down a @Union@.
restrict :: Typeable a => Union s -> Either (Union (s :\ a)) a
restrict (Union d) = maybe (Left $ Union d) Right $ fromDynamic d
{-# INLINE restrict #-}

-- | Generalize a @Union@.
reUnion :: (s :< s') => Union s -> Union s'
reUnion (Union d) = Union d
{-# INLINE reUnion #-}

-- | Use this in places where all the @Union@ed options have been exhausted.
typesExhausted :: Union '[] -> a
typesExhausted = error "Union types exhausted - empty Union"
{-# INLINE typesExhausted #-}

