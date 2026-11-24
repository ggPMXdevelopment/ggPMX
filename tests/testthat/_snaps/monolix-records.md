# Monolix record joins warn and continue with incompatible repeated-key counts

    Code
      result <- merge_monolix_records(x, y, c("ID", "TIME"))
    Condition
      Warning:
      Repeated Monolix observation counts differ at shared keys; matching by within-key order and dropping unmatched records. Observation pairing may be ambiguous.

---

    Code
      loaded <- post_load(list(predictions = x), y, "mlx18", NULL, "")
    Condition
      Warning:
      Repeated Monolix observation counts differ at shared keys; matching by within-key order and dropping unmatched records. Observation pairing may be ambiguous.
    Message
      NO FINEGRID FILE:
              we will use instead predictions.txt for individual plots
