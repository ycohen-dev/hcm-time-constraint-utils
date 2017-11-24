interface ZIF_HR_DELETE_COLISION_RSLTS
  public .

  METHODS get_extended_record
    RETURNING VALUE(rrs_extended_record) TYPE REF TO data.

endinterface.
