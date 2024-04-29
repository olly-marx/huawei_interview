Require Import Arith.

Definition is_perfect_square n : Prop :=
  exists m : nat, m * m = n.


Lemma sqrt_of_perfect_square_is_rational : forall k : nat,
  (exists p q, q <> 0 /\ k = (p  / q) * (p / q)) -> (is_perfect_square k).
Proof.
  intros k H.
  destruct H as [p [q H]].
  unfold is_perfect_square.
  exists (p / q).
  destruct H.
  simpl in H0.
  inversion H0.
  reflexivity.
Qed.

Lemma sqrt_of_perfect_square_is_rational' : forall k,
  (is_perfect_square k) -> (exists p q, q <> 0 /\ k = (p / q) * (p / q)).
  intros k H.
  unfold is_perfect_square in H.
  destruct H as [m Hm].
  exists m. exists 1.
  split.
  - discriminate.
  - inversion Hm.
    rewrite Nat.div_1_r. reflexivity.
Qed.

Lemma sqrt_of_perfect_square_iff_rational : forall k : nat,
  (exists p q, q <> 0 /\ k = (p / q) * (p / q)) <-> (is_perfect_square k).
Proof.
  intros k.
  split.
  - (* Forward direction *)
    apply sqrt_of_perfect_square_is_rational.
  - (* Backward direction *)
    apply sqrt_of_perfect_square_is_rational'.
Qed.

