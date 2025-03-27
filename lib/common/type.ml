open Printf

type conc_lab =
  | TLow
  | THigh

type sec_lab =
  | TStar
  | TConc of conc_label

type base_type =
  | TUnit
  | TBool

type raw_type =
  | TBase of base_type
  | TArrow of ttype * ttype * sec_label

and ttype =
  | Type of raw_type * sec_label