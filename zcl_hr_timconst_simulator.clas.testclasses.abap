*"* use this source file for your ABAP unit test classes
CLASS ltcl_timconst_simulator DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.

    TYPES: BEGIN OF ts_pernr_dates,
             pernr  TYPE numc07,
             begda  TYPE begda,
             endda  TYPE endda,
             nonkey TYPE string,
           END OF ts_pernr_dates,

           tt_pernr_dates TYPE STANDARD TABLE OF ts_pernr_dates.

    DATA: mo_simulator TYPE REF TO zif_hr_timconst_simulator,
          mv_lines_before_ops TYPE i.

    METHODS:
      setup,
      detect_delimitation FOR TESTING RAISING cx_static_check,
      detect_overriden FOR TESTING RAISING cx_static_check,
      keyfield_aware   FOR TESTING RAISING cx_static_check.
ENDCLASS.


CLASS ltcl_timconst_simulator IMPLEMENTATION.

  METHOD detect_delimitation.

    DATA: ls_insert_date       TYPE me->ts_pernr_dates,
          lo_collision_result  TYPE REF TO zif_hr_modify_colision_rslts,
          lrs_delimited_record TYPE REF TO me->ts_pernr_dates,
          lrt_resolved_table   TYPE REF TO data.

    FIELD-SYMBOLS: <lt_resolved_table> TYPE me->tt_pernr_dates.

    mo_simulator->set_time_constraints(
      EXPORTING
        iv_allow_concurent_records = abap_false
        iv_allow_gaps              = abap_false
    ).

    ls_insert_date = VALUE #( pernr = '00000001' begda = '20170201' endda = '99991231' ).

    lo_collision_result =  mo_simulator->check_collision_insert( is_record =  ls_insert_date ).

    lrs_delimited_record ?= lo_collision_result->get_delimited_record( ).

    cl_abap_unit_assert=>assert_bound( act = lrs_delimited_record ).

    cl_abap_unit_assert=>assert_equals( exp = '20170101'  act = lrs_delimited_record->begda ).
    cl_abap_unit_assert=>assert_equals( exp = '99991231'  act = lrs_delimited_record->endda ).
    cl_abap_unit_assert=>assert_equals( exp = '0000001'  act = lrs_delimited_record->pernr ).

    mo_simulator->insert_record( is_record = ls_insert_date ).

    lrt_resolved_table ?= mo_simulator->get_resolved_table( ).
    ASSIGN lrt_resolved_table->* TO <lt_resolved_table>.

    IF NOT line_exists( <lt_resolved_table>[ pernr = '0000001'
                                             begda = '20170101'
                                             endda = '20170131' ] ).

      cl_abap_unit_assert=>fail(  msg = 'Failed to resolve delimination insertion' ).

    ENDIF.

  ENDMETHOD.

  METHOD setup.

    DATA: lt_pernr_dates TYPE me->tt_pernr_dates,
          lt_key_fields  TYPE zif_hr_timconst_simulator=>tt_key_fields.

    lt_key_fields = VALUE #( ( 'PERNR' ) ).

    lt_pernr_dates = VALUE #(
        ( pernr = '0000001' begda = '20170101' endda = '99991231' nonkey = 'A' )
        ( pernr = '0000001' begda = '20160101' endda = '20161231' nonkey = 'B' )
        ( pernr = '0000001' begda = '20150101' endda = '20151231' nonkey = 'C' )

        ( pernr = '0000002' begda = '20170101' endda = '99991231' nonkey = 'A' )
        ( pernr = '0000002' begda = '20160101' endda = '20161231' nonkey = 'B' )
        ( pernr = '0000002' begda = '20150101' endda = '20151231' nonkey = 'C' )
    ).

    mv_lines_before_ops = lines( lt_pernr_dates ).

    mo_simulator = NEW zcl_hr_timconst_simulator( ).

    mo_simulator->set_table( it_table = lt_pernr_dates ).

  ENDMETHOD.

  METHOD detect_overriden.

    DATA: ls_insert_date TYPE me->ts_pernr_dates,
          lo_collision_result TYPE REF TO zif_hr_modify_colision_rslts,
          lrt_overriden_records TYPE REF TO data,
          lrt_resolved_table TYPE REF TO data.

    FIELD-SYMBOLS: <lt_overriden_records> TYPE me->tt_pernr_dates,
                   <lt_resolved_table> TYPE me->tt_pernr_dates.

    mo_simulator->set_time_constraints(
      EXPORTING
        iv_allow_concurent_records = abap_false
        iv_allow_gaps              = abap_false
    ).

    ls_insert_date = VALUE #( pernr = '00000001' begda = '20150101' endda = '20151231' nonkey = 'Z' ).

    lo_collision_result =  mo_simulator->check_collision_insert( is_record =  ls_insert_date ).

    lrt_overriden_records ?= lo_collision_result->get_overriden_records(  ).

    cl_abap_unit_assert=>assert_bound( act = lrt_overriden_records ).

    ASSIGN lrt_overriden_records->* TO <lt_overriden_records>.

    cl_abap_unit_assert=>assert_equals(
        msg = 'Unexpected number of overriden records'
        exp = 1
        act = lines( <lt_overriden_records> )
    ).

    cl_abap_unit_assert=>assert_equals(
        msg = 'Wrong line overriden'
        exp = 'C'
        act = <lt_overriden_records>[ 1 ]-nonkey
    ).

    mo_simulator->insert_record( is_record = ls_insert_date ).
    lrt_resolved_table = mo_simulator->get_resolved_table( ).

    cl_abap_unit_assert=>assert_bound( act = lrt_resolved_table ).

    ASSIGN lrt_resolved_table->* TO <lt_resolved_table>.

    cl_abap_unit_assert=>assert_equals(
        msg = 'Unexpected number of lines'
        exp = mv_lines_before_ops
        act = lines( <lt_resolved_table> )
     ).

     IF NOT line_exists( <lt_resolved_table>[ pernr = '0000001'
                                              begda = '20150101'
                                              endda = '20151231'
                                              nonkey = 'Z'
                                             ] ).

      cl_abap_unit_assert=>fail(  msg = 'Failed to resolve ovveride insertion' ).

    ENDIF.

  ENDMETHOD.

  METHOD keyfield_aware.

  DATA:   ls_insert_date TYPE me->ts_pernr_dates,
          lo_collision_result TYPE REF TO zif_hr_modify_colision_rslts,
          lrt_resolved_table TYPE REF TO data.

    FIELD-SYMBOLS: <lt_resolved_table> TYPE me->tt_pernr_dates.

    mo_simulator->set_time_constraints(
      EXPORTING
        iv_allow_concurent_records = abap_false
        iv_allow_gaps              = abap_false
    ).

    ls_insert_date = VALUE #( pernr = '00000003' begda = '20150101' endda = '20151231' nonkey = 'C' ).

    lo_collision_result =  mo_simulator->check_collision_insert( is_record =  ls_insert_date ).

    cl_abap_unit_assert=>assert_not_bound( act = lo_collision_result->get_delimited_record(  ) ).
    cl_abap_unit_assert=>assert_not_bound( act = lo_collision_result->get_overriden_records(  ) ).
    cl_abap_unit_assert=>assert_not_bound( act = lo_collision_result->get_postponded_record(  ) ).

    mo_simulator->insert_record( is_record = ls_insert_date ).
    lrt_resolved_table = mo_simulator->get_resolved_table( ).

    cl_abap_unit_assert=>assert_bound( act = lrt_resolved_table ).

    ASSIGN lrt_resolved_table->* TO <lt_resolved_table>.

    cl_abap_unit_assert=>assert_equals(
        msg = 'Unexpected number of lines'
        exp = mv_lines_before_ops + 1
        act = lines( <lt_resolved_table> )
     ).


  ENDMETHOD.

ENDCLASS.
