CLASS zcrgol_2610_field DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    TYPES: BEGIN OF ys_table,
             x    TYPE i,
             y    TYPE i,
             cell TYPE abap_bool, "TYPE REF TO zcrgol_2610_cell,
           END OF ys_table,
           yt_table TYPE STANDARD TABLE OF ys_table
               WITH DEFAULT KEY.

    DATA: gt_table TYPE yt_table.

    METHODS create_table IMPORTING x TYPE i
                                   y TYPE i.
    METHODS display_table.
    METHODS getaliveneighbs.
  PROTECTED SECTION.
    METHODS add_line
      IMPORTING
        iv_x   TYPE i
        io_ran TYPE REF TO cl_abap_random_int.
    METHODS get_random
      RETURNING
        VALUE(ro_ran) TYPE REF TO cl_abap_random_int
      RAISING
        cx_abap_random.
    METHODS new_line
      IMPORTING
        i_line TYPE zcrgol_2610_field=>ys_table
       CHANGING
        i_x TYPE i.
    METHODS write_value
      IMPORTING
        i_line TYPE zcrgol_2610_field=>ys_table.
    METHODS count
      IMPORTING
        i_cell            TYPE zcrgol_2610_field=>ys_table
      RETURNING
        VALUE(rv_counter) TYPE i.
    TYPES:
      ty_lt_table TYPE yt_table.
    METHODS set_new_value
      IMPORTING
        i_cell          TYPE zcrgol_2610_field=>ys_table
        iv_counter      TYPE i
      CHANGING
        VALUE(rt_table) TYPE ty_lt_table.
  PRIVATE SECTION.


ENDCLASS.



CLASS zcrgol_2610_field IMPLEMENTATION.


  METHOD create_table.

    DATA: lv_seed TYPE i.

    DATA lo_ran TYPE REF TO cl_abap_random_int.


    lo_ran = get_random( ).

    DO x TIMES.

      DATA(lv_x) = sy-index.

      DO y TIMES.


        add_line(
              iv_x   = lv_x
              io_ran = lo_ran ).

      ENDDO.
    ENDDO.
  ENDMETHOD.


  METHOD display_table.

    data: lv_x type i.

    LOOP AT gt_table ASSIGNING FIELD-SYMBOL(<line>).
      "new_line( <line> ).
      new_line(
        EXPORTING
          i_line = <line>
        CHANGING
          i_x    = lv_x
      ).

      write_value( <line> ).

    ENDLOOP.

    WRITE: /.
  ENDMETHOD.

  METHOD getaliveneighbs.
    DATA lv_counter TYPE i.
    DATA: lt_table TYPE yt_table.
    LOOP AT gt_table ASSIGNING FIELD-SYMBOL(<cell>).
      CLEAR lv_counter.

      lv_counter = count( <cell> ).

      set_new_value(
        EXPORTING
          i_cell     = <cell>
          iv_counter = lv_counter
        CHANGING
          rt_table   = lt_table
      ).

    ENDLOOP.

    CLEAR gt_table.
    gt_table[] = lt_table[].
  ENDMETHOD.


  METHOD add_line.


    DATA:lv_live TYPE abap_bool.


    DATA(lv_i) = io_ran->get_next( ).

    IF lv_i MOD 2 = 0.
      DATA(live) = abap_false.
    ELSE.
      live = abap_true.
    ENDIF.

    APPEND VALUE #( x = iv_x y = sy-index cell = live ) TO gt_table.

  ENDMETHOD.


  METHOD get_random.

    DATA lv_seed TYPE i.

    lv_seed = sy-timlo.

    ro_ran  = cl_abap_random_int=>create( min = 1 max = 100 seed = lv_seed ).

  ENDMETHOD.


  METHOD new_line.

    "DATA temp_x TYPE i.

    IF i_line-x <> i_x.


      NEW-LINE.

      i_x = i_line-x.
    ENDIF.

  ENDMETHOD.


  METHOD write_value.

    IF i_line-cell = abap_false.
      WRITE: '0'.
    ELSE.
      WRITE: '1'.
    ENDIF.

  ENDMETHOD.


  METHOD count.

    DO 3 TIMES.
      DATA(lv_x) = i_cell-x + sy-index - 2.
      DO 3 TIMES.
        DATA(lv_y) = i_cell-y + sy-index - 2.

        IF lv_x LE 0 OR
           lv_y LE 0 OR
           (  lv_x EQ i_cell-x AND
              lv_y EQ i_cell-y ).
          CONTINUE.
        ENDIF.

        TRY.
            DATA(ls_chek) = gt_table[ x = lv_x  y = lv_y ].
          CATCH cx_sy_itab_line_not_found.
            CONTINUE.
        ENDTRY.


        IF ls_chek-cell = abap_true.
          rv_counter = rv_counter + 1.
        ENDIF.


      ENDDO.
    ENDDO.

  ENDMETHOD.


  METHOD set_new_value.

    IF i_cell-cell = abap_true.
      IF iv_counter < 2 OR iv_counter > 3.
        APPEND VALUE #( x = i_cell-x y = i_cell-y cell = abap_false ) TO rt_table.
      ELSE.
        APPEND VALUE #( x = i_cell-x y = i_cell-y cell = abap_true ) TO rt_table.
      ENDIF.
    ELSE.
      IF iv_counter = 3.
        APPEND VALUE #( x = i_cell-x y = i_cell-y cell = abap_true ) TO rt_table.
      ELSE.

        APPEND VALUE #( x = i_cell-x y = i_cell-y cell = abap_false ) TO rt_table.
      ENDIF.
    ENDIF.

  ENDMETHOD.

ENDCLASS.
