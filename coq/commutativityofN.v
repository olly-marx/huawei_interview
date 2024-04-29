Require Import PeanoNat.

Lemma plus_comm : forall n m : nat,
  n + m = m + n.
Proof.
  intros n m.
  induction n.
  - rewrite Nat.add_0_r.
    reflexivity.
  - simpl.
    rewrite IHn.
    rewrite <- Nat.add_succ_comm.
    reflexivity.
Qed.

