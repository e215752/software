module lemma6 where

-- 論理型を定義
data _∨_ (A B : Set) : Set where
  inl : A → A ∨ B
  inr : B → A ∨ B

-- `B → (A ∨ (B → A)) → A` 型の証明
lemma : ∀ {A B : Set} (b : B) (f : A ∨ (B → A)) → A
lemma b (inl a) = a
lemma b (inr bToA) = bToA b

-- `example` という証明
example : ∀ {A B : Set} (b : B) (f : A ∨ (B → A)) → A
example b (inl a) = a
example b (inr bToA) = bToA b









