
--  with Ada.Text_IO;    use Ada.Text_IO;
with Gval;     use Gval;
with Glib;  --  use Glib;
with Ada.Numerics.Elementary_Functions;
use Ada.Numerics.Elementary_Functions;

package body Gui is
   use type Glib.Gdouble;  --  To be available to use operators *,/,+,-,...
--
   procedure Gui_Initialize_Left_End is
   begin
         L_Z_Pix := S_Y_Pix + L_Z_Pix_Factor * Sqrt (L_Z_M);
         S_X_Pos_M := 0.0;            --  Calculation Value meter
         S_X_Pos_Pix := S_X_Space;    --  Calculation Value pixel
         Progress := 0.0;
   end Gui_Initialize_Left_End;
--
   procedure Gui_Initialize_Right_End is
   begin
         L_Z_Pix := S_Y_Pix + L_Z_Pix_Factor * Sqrt (L_Z_M);
         S_X_Pos_M := S_X_Dist_M;                 --  Calculation Value meter
         S_X_Pos_Pix := S_X_Dist_Pix + S_X_Space; --  Calculation Value pixel
         Progress := 0.0;
   end Gui_Initialize_Right_End;
--
--
   procedure Gui_Play is
   begin
      null;
   end Gui_Play;
--
--
   procedure Gui_End is
   begin
      null;
   end Gui_End;
--
--
--------------------------------------
--  DRAW GUI DISPLAY
--------------------------------------
   function On_Draw
     (Widget : access Gtk.Drawing_Area.Gtk_Drawing_Area_Record'Class;
      Cr     : Cairo.Cairo_Context) return Boolean is
      pragma Unreferenced (Widget);

   begin
      --  Clear background, Paint all painting area with White
      Cairo.Set_Source_Rgb (Cr, 1.0, 1.0, 1.0);
      Cairo.Paint (Cr);

      --  Draw movement X path line
      Cairo.Set_Source_Rgb (Cr, 0.7, 0.7, 0.7);
      Cairo.Set_Line_Width (Cr, 2.0);
      Cairo.Move_To (Cr, S_X_Space, S_Y_Pix);    --  Start point = Left
      Cairo.Line_To (Cr,
                     S_X_Space + S_X_Dist_Pix,   --  Right X End Point
                     S_Y_Pix);
      --  Draw Y (Vertical line)
      Cairo.Move_To (Cr, L_X_Pix, S_Y_Pix - 50.0);  --  Y Plus
      Cairo.Line_To (Cr, L_X_Pix, S_Y_Pix + 50.0);  --  Y Minus

      --  Draw Z (Diagonal line)
      Cairo.Move_To (Cr, L_X_Pix + 30.0 / 1.73, 40.0);
      Cairo.Line_To (Cr, L_X_Pix - 80.0 / 1.73, 150.0);

      Cairo.Stroke (Cr);

      --  Draw fixed Listener (Red)
      Cairo.Set_Source_Rgb (Cr, 1.0, 0.0, 0.0);
      Cairo.Arc (Cr,
         Glib.Gdouble (L_X_Pix - (L_Z_Pix - S_Y_Pix) / 1.73), --  X Center
         Glib.Gdouble (L_Z_Pix),                       --  Y Center
         L_Radius,
         0.0, 2.0 * Ada.Numerics.Pi);    --  Angle1, Angle2 (Circle)
      Cairo.Fill (Cr);

      --  Draw moving sound Source (Blue) Rectangle
      Cairo.Set_Source_Rgb (Cr, 0.0, 0.0, 1.0);
      Cairo.Rectangle (Cr,
         Glib.Gdouble (S_X_Pos_Pix - S_X_Len_Pix / 2.0),       --  X Left
         Glib.Gdouble (S_Y_Pix - S_Radius / 2.0 - S_Radius / 2.0), --  Y Top
         Glib.Gdouble (S_X_Len_Pix),                           --  Width
         Glib.Gdouble (S_Radius * 2.0));                       --  Height
      Cairo.Fill (Cr);

      --  Draw Souce markers
      Cairo.Set_Source_RGB (Cr, 0.0, 1.0, 0.0);
      for nnn in 1 .. Natural (S_Count) loop
         Cairo.Arc
           (Cr => Cr,
            Xc => Glib.Gdouble
              (S_X_Pos_Pix + S_X_Len_Pix / 2.0 -
                   S_X_Len_Pix / (S_Count - 1.0) * Float (nnn - 1)),
            Yc => Glib.Gdouble (S_Y_Pix),
            Radius => Glib.Gdouble (S_Radius / 2.0),
            Angle1 => 0.0,
            Angle2 => Glib.Gdouble (2.0 * Ada.Numerics.Pi));
      end loop;
      Cairo.Fill (Cr);
--
------  TEXT  -----------------
--  Set Font
      Cairo.Select_Font_Face (Cr, "Arial",
                             Cairo.Cairo_Font_Slant_Normal,
                             Cairo.Cairo_Font_Weight_Bold);
      Cairo.Set_Font_Size (Cr, 14.0);
--  Font Color
      Cairo.Set_Source_Rgb (Cr, 0.0, 0.0, 0.0);  --  Black
      --  +X
      Cairo.Move_To (Cr, S_X_Space + S_X_Dist_Pix + 10.0, S_Y_Pix);
      Cairo.Show_Text (Cr, "+X");
      --  +Y
      Cairo.Move_To (Cr, L_X_Pix, 15.0);
      Cairo.Show_Text (Cr, "+Y");
      --  +Z
      Cairo.Move_To (Cr, L_X_Pix + 25.0, 35.0);
      Cairo.Show_Text (Cr, "+Z");
      --  "Listner"
      Cairo.Move_To
        (Cr, Glib.Gdouble (L_X_Pix - (L_Z_Pix - S_Y_Pix) / 1.73 + 15.0),
         Glib.Gdouble (L_Z_Pix + 15.0));
      Cairo.Show_Text (Cr, "Listener");
      --  "Source"
      Cairo.Move_To (Cr, Glib.Gdouble (S_X_Pos_Pix - 25.0),
                     Glib.Gdouble (S_Y_Pix - 17.0));
      Cairo.Show_Text (Cr, "Source");

      return False;
   end On_Draw;
--
--
--

end Gui;
