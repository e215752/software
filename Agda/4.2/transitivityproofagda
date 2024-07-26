module TransitivityProof where

open import Agda.Builtin.Equality

-- 同値性の推移性の証明
transitivity : {A : Set} {x y z : A} → x ≡ y → y ≡ z → x ≡ z
transitivity refl refl' = refl' 
