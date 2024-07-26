module injectivityproof where

open import Relation.Binary.PropositionalEquality as Eq
open Eq using (_≡_; refl; cong; sym)
open Eq.≡-Reasoning
open import Function.Base using (_∘_)
open import Function.Definitions using (Injective)

injectivity : ∀ {A B : Set} {f : A → B} → Injective _≡_ _≡_ f → ∀ {x y} → f x ≡ f y → x ≡ y
injectivity inj fx≡fy = inj fx≡fy