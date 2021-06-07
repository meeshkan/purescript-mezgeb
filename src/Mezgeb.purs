module Mezgeb where

import Data.Symbol (class IsSymbol)
import Data.Tuple (snd)
import Data.Tuple.Nested (type (/\), (/\))
import Heterogeneous.Folding (class FoldingWithIndex, class HFoldlWithIndex, hfoldlWithIndex)
import Prim.Row (class Lacks, class Cons)
import Record as Record
import Type.Proxy (Proxy)



data MezgebGet
  = MezgebGet

instance mezgebGet ::
  (IsSymbol l, Cons l x i' i, Lacks l o', Cons l x o' o) =>
  FoldingWithIndex MezgebGet (Proxy l) ({ | i } /\ { | o' }) ignore ({ | i } /\ { | o }) where
  foldingWithIndex MezgebGet prop (i /\ o') _ = i /\ Record.insert prop (Record.get prop i) o'

get ::
  forall i r o.
  HFoldlWithIndex MezgebGet ({ | i } /\ {}) { | r } ({ | i } /\ { | o }) =>
  { | r } -> { | i } -> { | o }
get r i = snd (hfoldlWithIndex MezgebGet (i /\ {}) r)

data MezgebSet
  = MezgebSet

instance mezgebSet ::
  (IsSymbol l, Cons l a r i, Cons l b r o) =>
  FoldingWithIndex MezgebSet (Proxy l) { | i } b { | o } where
  foldingWithIndex MezgebSet prop i b = Record.set prop b i

set ::
  forall i r o.
  HFoldlWithIndex MezgebSet { | i } { | r } { | o } =>
  { | r } -> { | i } -> { | o }
set r i = hfoldlWithIndex MezgebSet i r

data MezgebModify
  = MezgebModify

instance mezgebModify ::
  (IsSymbol l, Cons l a r i, Cons l b r o) =>
  FoldingWithIndex MezgebModify (Proxy l) { | i } (a -> b) { | o } where
  foldingWithIndex MezgebModify prop i ab = Record.modify prop ab i

modify ::
  forall i r o.
  HFoldlWithIndex MezgebModify { | i } { | r } { | o } =>
  { | r } -> { | i } -> { | o }
modify r i = hfoldlWithIndex MezgebModify i r

data MezgebInsert
  = MezgebInsert

instance mezgebInsert ::
  (IsSymbol l, Lacks l i, Cons l a i o) =>
  FoldingWithIndex MezgebInsert (Proxy l) { | i } a { | o } where
  foldingWithIndex MezgebInsert prop i a = Record.insert prop a i

insert ::
  forall i r o.
  HFoldlWithIndex MezgebInsert { | i } { | r } { | o } =>
  { | r } -> { | i } -> { | o }
insert r i = hfoldlWithIndex MezgebInsert i r

data MezgebDelete
  = MezgebDelete

instance mezgebDelete ::
  (IsSymbol l, Lacks l o, Cons l x o i) =>
  FoldingWithIndex MezgebDelete (Proxy l) { | i } ignore { | o } where
  foldingWithIndex MezgebDelete prop i _ = Record.delete prop i

delete ::
  forall i r o.
  HFoldlWithIndex MezgebDelete { | i } { | r } { | o } =>
  { | r } -> { | i } -> { | o }
delete r i = hfoldlWithIndex MezgebDelete i r
