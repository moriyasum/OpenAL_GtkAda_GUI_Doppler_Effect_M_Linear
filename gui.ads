
with Gtk.Drawing_Area;  --  use Gtk.Drawing_Area;
with Cairo;

package Gui is

   procedure Gui_Initialize_Left_End;
   procedure Gui_Initialize_Right_End;

   procedure Gui_Play;

   procedure Gui_End;
--
   function On_Draw
     (Widget : access Gtk.Drawing_Area.Gtk_Drawing_Area_Record'Class;
      Cr     : Cairo.Cairo_Context) return Boolean;
--
end Gui;
