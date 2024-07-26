module CategoryTheory where

open import Agda.Builtin.Equality
open import Agda.Builtin.Unit
open import Data.Product
open import Relation.Binary.PropositionalEquality.Core

-- 射の合成
_○_ : {A B C : Set} → (g : B → C) → (f : A → B) → A → C
g ○ f = λ x → g (f x)

-- 合成の結合則の証明
assoc : {A B C D : Set} → (f : C → D) → (g : B → C) → (h : A → B) → (f ○ (g ○ h)) ≡ ((f ○ g) ○ h)
assoc f g h = refl

-- -- Sets の圏の定義
-- Sets : Category
-- Sets = record
--   { Obj = Set
--   ; Hom = λ a b → a → b
--   ; _o_ = λ f g x → f (g x)
--   ; id = λ A x → x
--   ; isCategory = record
--       { idL = λ {A B} (f : A → B) → extensionality (λ x → id (f x)) refl
--       ; idR = λ {A B} (f : A → B) → extensionality (λ x → id (f x)) refl
--       ; assoc = assoc
--       }
--   }
