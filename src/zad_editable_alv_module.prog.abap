*&---------------------------------------------------------------------*
*& Include          ZBOOK_EDITABLE_ALV_MODULE
*&---------------------------------------------------------------------*
MODULE status_0100 OUTPUT.
  SET PF-STATUS '0100'.
  PERFORM f_create_objects.
ENDMODULE.

MODULE at_exit_command_0100 INPUT.
  LEAVE TO SCREEN 0.
ENDMODULE.

MODULE user_command_0100 INPUT.
  PERFORM f_user_command.
ENDMODULE.
