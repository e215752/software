module FunctionEquality where

open import Agda.Builtin.Equality

-- 同値性の保たれる関数
functionEquality : {A B : Set} {x y : A} {f : A → B} → x ≡ y → f x ≡ f y
functionEquality refl = refl
