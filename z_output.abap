REPORT z_output.
START-OF-SELECTION.

    parameters: pa_x type i,
                pa_y type i,
                pa_times type i.

   DATA(field) = NEW zcrgol_2610_field( ).
   field->create_table(
     EXPORTING
       x = pa_x
       y = pa_y
   ).


   field->display_table( ).
   do pa_times times.
   field->getaliveneighbs( ).
   field->display_table( ).
   enddo.
END-OF-SELECTION.
