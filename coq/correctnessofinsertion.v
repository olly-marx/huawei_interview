Require Import List.
Import ListNotations.
Require Import Arith.
Require Import Lia.

(* Define the insertion sort algorithm *)
Fixpoint insert (x : nat) (lst : list nat) : list nat :=
  match lst with
  | [] => [x]
  | y :: ys => if leb x y then x :: lst else y :: insert x ys
  end.

Fixpoint insertion_sort (lst : list nat) : list nat :=
  match lst with
  | [] => []
  | x :: xs => insert x (insertion_sort xs)
  end.

(* Define a function to test insertion sort *)
Definition test_insertion_sort : bool :=
  let unsorted := [3; 1; 4; 1; 5; 9; 2; 6; 5] in
  let sorted := [1; 1; 2; 3; 4; 5; 5; 6; 9] in
  Nat.eqb (length (insertion_sort unsorted)) (length sorted) &&
  forallb (fun x => Nat.eqb (nth x (insertion_sort unsorted) 0) (nth x sorted 0))
          (seq 0 (length unsorted)).

(* Test the insertion sort function *)
Compute insertion_sort [3; 1; 4; 1; 5; 9; 2; 6; 5].

(* Define a specification for correctness *)
Definition sorted (lst : list nat) :=
  forall i j, i <= j <= length lst -> nth i lst 0 <= nth j lst 0.

Lemma insert_increases_length : forall x lst,
  length (insert x lst) = S (length lst).
Proof.
  intros x lst.
  induction lst as [| y ys IHys].
  - (* Case: lst is empty *)
    simpl.
    reflexivity.
  - (* Case: lst is not empty *)
    simpl.
    destruct (leb x y) eqn:H_le.
    + (* Case: x <= y *)
      reflexivity.
    + (* Case: x > y *)
      rewrite <- IHys.
      reflexivity.
Qed.

Lemma insertion_sort_preserves_length : forall x xs,
  S (length (insertion_sort xs)) = (length (insertion_sort (x :: xs))).
Proof.
  intros x xs.
  simpl.
  rewrite insert_increases_length.
  (* Now we have length (insertion_sort xs) <= S (length (insertion_sort xs)) *)
  (* Which is trivially true *)
  reflexivity.
Qed.

Lemma not_le_0_iff_gt_0 : forall i : nat,
  ~ (0 <= i) <-> i < 0.
Proof.
  intros i.
  split.
  - (* -> *)
    intros H_not_le_0.
    destruct (Nat.le_gt_cases 0 i) as [H_i_gt_0 | H_i_le_0].
    + (* Case: i > 0 *)
      contradiction.
    + (* Case: i <= 0 *)
      apply H_i_le_0.
  - (* <- *)
    intros H_gt_0 H_le_0.
    inversion H_le_0.
    rewrite <- H in H_gt_0.
    lia. lia.
Qed.


Lemma insertion_sort_correct : forall (lst : list nat),
  sorted (insertion_sort lst).
Proof.
  intros lst i j.
  induction lst.
  - unfold insertion_sort, sorted.
    intros.
    unfold length in H.
    destruct H as [Hi Hj].
    apply Nat.le_0_r in Hj.
    rewrite Hj in Hi.
    apply Nat.le_0_r in Hi.
    rewrite Hi.
    rewrite Hj.
    simpl.
    apply le_n.
  - destruct i, j.
    destruct IHlst.
    + split.
      * apply Nat.le_0_r. reflexivity.
      * lia.
    + intros.
      apply le_n. 
    + intros.
      apply le_n.
    + (* Case: i > 0, j = 0 *)
    (* This case is impossible since i is greater than 0. *)
    (* You can use Nat.nle_gt to derive a contradiction. *)
      intros.
      admit.
    + destruct (Nat.le_gt_cases 0 i) as [H_i_gt_0 | H_i_le_0].
      destruct IHlst.
      admit.
    + (* Case: i > 0, j > 0 *)
    (* You need to prove S i <= S j <= length (insertion_sort (x :: xs)) -> ... *)
    (* This case is the most interesting one. You need to utilize the IHxs here. *)
    (* Hint: Think about how the sorting algorithm affects the ordering of elements in the list. *)
    (* You can use the insert lemma to split the proof into cases based on whether x should be inserted at the head or tail of the sorted sublist. *)
      admit.

Abort.
