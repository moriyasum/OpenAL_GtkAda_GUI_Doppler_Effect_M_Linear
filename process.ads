--  with Audio;
--  with Gui;
with Gtk.Handlers;   use Gtk.Handlers;
with Gtk.Widget;     use Gtk.Widget;
with Gtk.Button;     use Gtk.Button;
with Glib.Object;    --  use Glib.Object;
with Gdk.Event;      use Gdk.Event;
with Gtk.GEntry;     use Gtk.GEntry;
--
package Process is

--  Push Button Callback -----
   type Callback_Data is record
      Number : Integer;
      --  Label : Gtk.Label.Gtk_Label;
      --  The other parameters if neccessary
   end record;
   type Callback_Data_Access is access all Callback_Data;
   --
   package User_Callback is new Gtk.Handlers.User_Callback
     (Gtk_Button_Record, Callback_Data_Access);

   procedure On_Button_Play_Cb
     (Button : access Gtk_Button_Record'Class;
      Data  : Callback_Data_Access);

   procedure On_Button_Pause_Cb
     (Button : access Gtk_Button_Record'Class;
      Data  : Callback_Data_Access);

   procedure On_Button_Stop_Cb
     (Button : access Gtk_Button_Record'Class;
      Data  : Callback_Data_Access);
--
--  Radio Button Callback --------------
   type Radio_Data is record
      Button_Name : String (1 .. 10);
      Name_Length : Natural;
   end record;
   type Radio_Data_Access is access all Radio_Data;

   package Radio_Handlers is new Gtk.Handlers.User_Callback
     (Widget_Type => Gtk_Widget_Record,
      User_Type   => Radio_Data_Access);

   procedure On_Radio_Button_Cb
      (Widget : access Gtk_Widget_Record'Class;
       Data   : Radio_Data_Access);
---------------
--
-- File Chooser Callback ----------
   package Chooser_Callback is new Gtk.Handlers.Callback
     (Widget_Type => Gtk_Widget_Record);

   procedure On_File_Chooser_Cb
     (Widget : access Gtk.Widget.Gtk_Widget_Record'Class);
--
-----------------------
--  Entry Callback
-----------------------
   package Entry_Callback is new Gtk.Handlers.Callback    --  Entry "Return"
     (Gtk.GEntry.Gtk_Entry_Record);
   --
   package Widget_Event_Callback is new Gtk.Handlers.Return_Callback
      (Gtk.Widget.Gtk_Widget_Record,
       Boolean);
--  Speed
   procedure On_Entry_Speed_Cb
     (Widget : access Gtk.GEntry.Gtk_Entry_Record'Class);
   function On_Speed_Focus_Out_Handler_Cb
     (Widget : access Gtk.Widget.Gtk_Widget_Record'Class;
      Event  : Gdk.Event.Gdk_Event) return Boolean;
--
--  Distance
   procedure On_Entry_Distance_Cb
     (Widget : access Gtk.GEntry.Gtk_Entry_Record'Class);
   function On_Distance_Focus_Out_Handler_Cb
     (Widget : access Gtk.Widget.Gtk_Widget_Record'Class;
      Event  : Gdk.Event.Gdk_Event) return Boolean;

--  Offset
   procedure On_Entry_Offset_Cb
     (Widget : access Gtk.GEntry.Gtk_Entry_Record'Class);
   function On_Offset_Focus_Out_Handler_Cb
     (Widget : access Gtk.Widget.Gtk_Widget_Record'Class;
      Event  : Gdk.Event.Gdk_Event) return Boolean;

--  Source Length
   procedure On_Entry_Slength_Cb
     (Widget : access Gtk.GEntry.Gtk_Entry_Record'Class);
   function On_Slength_Focus_Out_Handler_Cb
     (Widget : access Gtk.Widget.Gtk_Widget_Record'Class;
      Event  : Gdk.Event.Gdk_Event) return Boolean;

--  Number of Source
   procedure On_Entry_SourceCount_Cb
     (Widget : access Gtk.GEntry.Gtk_Entry_Record'Class);
   function On_SourceCount_Focus_Out_Handler_Cb
     (Widget : access Gtk.Widget.Gtk_Widget_Record'Class;
      Event  : Gdk.Event.Gdk_Event) return Boolean;

--
--  Scale Volume Slider Call Back ----------
   package Scale_Volume_Callback is new Gtk.Handlers.Callback   --  Volume
     (Gtk.Widget.Gtk_Widget_Record);

   procedure On_S_Scale_Volume_Cb
     (Widget : access Gtk.Widget.Gtk_Widget_Record'Class);
--
   procedure On_L_Scale_Volume_Cb
     (Widget : access Gtk.Widget.Gtk_Widget_Record'Class);
--
--
   function Delete_Handler
     (Self  : access Glib.Object.GObject_Record'Class;
      Event : Gdk_Event) return Boolean;
--
   procedure Process_Initialize;
--
   function Timer_Intr (Dummy_Int : Integer) return Boolean;

   TimerIntrCounter : Integer := 0;
--
end Process;
