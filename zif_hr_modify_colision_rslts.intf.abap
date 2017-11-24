interface ZIF_HR_MODIFY_COLISION_RSLTS
  public .

  METHODS get_delimited_record
    RETURNING VALUE(rrs_delimited_record) TYPE REF TO data.

  METHODS get_overriden_records
    RETURNING VALUE(rrt_deleted_records) TYPE REF TO data.

  METHODS get_postponded_record
    RETURNING VALUE(rrs_postponded_record) TYPE REF TO data.

endinterface.
