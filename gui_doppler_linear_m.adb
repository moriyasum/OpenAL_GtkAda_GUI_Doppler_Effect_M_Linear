with Process;      use Process;
--  with Audio;        use Audio;
with Gui;          use Gui;
with Gval;         use Gval;
with Ada.Text_IO;   use Ada.Text_IO;
with Gtk.Main;    --  use Gtk.Main;
with Gtk.Window;   use Gtk.Window;
with Gtk.Box;      use Gtk.Box;
with Gtk.Button;   use Gtk.Button;
with Gtk.Toggle_Button;  use Gtk.Toggle_Button;
with Gtk.Radio_Button;   use Gtk.Radio_Button;
with Gtk.GEntry;   use Gtk.GEntry;
with Gtk.Label;    use Gtk.Label;
with Gtk.Drawing_Area;
with Gtk.Widget;    use Gtk.Widget;
with Gtk.File_Chooser_Button;  use Gtk.File_Chooser_Button;
with Gtk.File_Chooser;  use Gtk.File_Chooser;
with Glib;         use Glib;
with Glib.Main;    use Glib.Main;
with Ada.Numerics.Elementary_Functions;
use Ada.Numerics.Elementary_Functions;
with Ada.Strings.Fixed;  --  use Ada.Strings.Fixed;  --  for Grid
with Ada.Strings;  use Ada.Strings;                  --  for Grid
with Gtk.Scale;    use Gtk.Scale;
with Gtk.Enums;     use Gtk.Enums;
with Gtk.Grid;      use Gtk.Grid;
--  with Gtk.Handlers;  use Gtk.Handlers;

procedure Gui_Doppler_Linear_m is
--
   package Time_Cb  is new Glib.Main.Generic_Sources (Integer);  -- Timer Intr

   Win       : Gtk.Window.Gtk_Window;
   Vbox_Main : Gtk.Box.Gtk_Box;
   Hbox_1    : Gtk.Box.Gtk_Box;
   Vbox_1L, Vbox_1M, Vbox_1R    : Gtk.Box.Gtk_Box;
   Vbox_2    : Gtk.Box.Gtk_Box;
   Vbox_3    : Gtk.Box.Gtk_Box;

   --  Buttons
   Button_Play  : Gtk.Button.Gtk_Button;
   Button_Pause  : Gtk.Button.Gtk_Button;
   Button_Stop : Gtk.Button.Gtk_Button;
   Button_Play_Data : Callback_Data_Access;  --  Parameter Main -> Callback
   Button_Stop_Data : Callback_Data_Access;
   Button_Pause_Data : Callback_Data_Access;

   --  Parameter Input Grid, Label and Entry Box
   Entry_Grid : Gtk.Grid.Gtk_Grid;
   Entry_L1, Entry_L2, Entry_L3, Entry_L4, Entry_L5 : Gtk_Label;
   --  Entry Box
   Entry_Box_Speed    : Gtk.GEntry.Gtk_Entry;
   Entry_Box_Distance : Gtk.GEntry.Gtk_Entry;
   Entry_Box_Offset   : Gtk.GEntry.Gtk_Entry;
   Entry_Box_Slength  : Gtk.GEntry.Gtk_Entry;
   Entry_Box_SourceCount : Gtk.GEntry.Gtk_Entry;

   --  Radio buttons
   Radio_Right : Gtk.Radio_Button.Gtk_Radio_Button;
   Radio_Left  : Gtk.Radio_Button.Gtk_Radio_Button;
   Radio_Repeat  : Gtk.Radio_Button.Gtk_Radio_Button;
   Right_Data   : constant Radio_Data_Access := new Radio_Data;
   Left_Data    : constant Radio_Data_Access := new Radio_Data;
   Repeat_Data  : constant Radio_Data_Access := new Radio_Data;

   --  File chooser
   File_Chooser : Gtk.File_Chooser_Button.Gtk_File_Chooser_Button;
   Chooser_Flag : Boolean;

   --  Label
   Label_S_Vol, Label_L_Vol : Gtk_Label;

   Scale_S_Gain_Slider : Gtk.Scale.Gtk_Hscale;
   Scale_L_Gain_Slider : Gtk.Scale.Gtk_Hscale;

   Gid_Dummy   : G_Source_Id;     --  Timer interrupt
   Dummy_Int : Integer := 0;    --  Interrupt dummy parameter

begin
   Gtk.Main.Init;

   --  Window
   Gtk.Window.Gtk_New (Win);
   Win.Set_Title ("Doppler Effect Demo");
   Win.Set_Default_Size (1000, 600);

------------------------
--  Layout: V/H Boxes
------------------------
   Gtk.Box.Gtk_New_Vbox (Vbox_Main, Homogeneous => False, Spacing => 5);
   Win.Add (Vbox_Main);
   Gtk.Box.Gtk_New_Hbox (Hbox_1, Homogeneous => False, Spacing => 5);
   Vbox_Main.Add (Hbox_1);
   Gtk.Box.Gtk_New_Vbox (Vbox_1L, Homogeneous => False, Spacing => 5);
   Hbox_1.Add (Vbox_1L);
   Gtk.Box.Gtk_New_Vbox (Vbox_1M, Homogeneous => False, Spacing => 5);
   Hbox_1.Add (Vbox_1M);
   Gtk.Box.Gtk_New_Vbox (Vbox_1R, Homogeneous => False, Spacing => 5);
   Hbox_1.Add (Vbox_1R);

   Gtk.Box.Gtk_New_Vbox (Vbox_2, Homogeneous => False, Spacing => 5);
   Vbox_Main.Add (Vbox_2);
   Gtk.Box.Gtk_New_Vbox (Vbox_3, Homogeneous => False, Spacing => 5);
   Vbox_Main.Add (Vbox_3);
--
---------------------
--  Play / Stop / Pause buttons  (VBox-1L)
---------------------
   Gtk.Button.Gtk_New (Button_Play, "Play");
   Gtk.Widget.Set_Size_Request (Gtk_Widget (Button_Play), 50, 50);
   Gtk.Button.Gtk_New (Button_Pause, "Pause");
   Gtk.Widget.Set_Size_Request (Gtk_Widget (Button_Pause), 50, 50);
   Gtk.Button.Gtk_New (Button_Stop, "Stop");
   Gtk.Widget.Set_Size_Request (Gtk_Widget (Button_Stop), 50, 50);
   Vbox_1L.Pack_Start
     (Button_Play, Expand => False, Fill => False, Padding => 2);
   Vbox_1L.Pack_Start
     (Button_Pause, Expand => False, Fill => False, Padding => 2);
   Vbox_1L.Pack_Start
     (Button_Stop, Expand => False, Fill => False, Padding => 2);
--
--  Button Callback Parameters
   Button_Play_Data := new Process.Callback_Data;
   Button_Play_Data.Number := 1;
   Button_Pause_Data := new Process.Callback_Data;
   Button_Pause_Data.Number := 2;
   Button_Stop_Data := new Process.Callback_Data;
   Button_Stop_Data.Number := 3;
--
   Process.User_Callback.Connect   --  "Connect":GOOD, "Object_Connect":BAD
     (Button_Play,
      "clicked",
      Process.User_Callback.To_Marshaller (On_Button_Play_Cb'Access),
      Button_Play_Data);
--
   Process.User_Callback.Connect
     (Button_Stop,
      "clicked",
      Process.User_Callback.To_Marshaller (On_Button_Stop_Cb'Access),
      Button_Stop_Data);
--
   Process.User_Callback.Connect
     (Button_Pause,
      "clicked",
      Process.User_Callback.To_Marshaller (On_Button_Pause_Cb'Access),
      Button_Pause_Data);
--
--
--------------------
--  File chooser  (VBox-1L)
--------------------
   Gtk.File_Chooser_Button.Gtk_New
     (File_Chooser,
      "Select WAV File Chooser Open",
      Gtk.File_Chooser.Action_Open);  --  Open, Save, Select_Fld, Create_Fld

   Chooser_Flag := Gtk.File_Chooser_Button.Set_Current_Folder
     (Chooser => File_Chooser,
      Filename => "./");
   if not Chooser_Flag then
      Put_Line ("WRONG, Chooser_Flag = False");
   end if;

   Chooser_Flag := Gtk.File_Chooser_Button.Set_Filename
     (Chooser  => File_Chooser,
      Filename => "testsound.wav");
   if not Chooser_Flag then
      Put_Line ("WRONG, Set_Filename failed");
   end if;
--
   Chooser_Callback.Connect
     (File_Chooser,
      "file-set",
      Chooser_Callback.To_Marshaller (On_File_Chooser_Cb'Access));
--
   Vbox_1L.Pack_Start
     (File_Chooser, Expand => False, Fill => False, Padding => 5);
--
-----------------------
--  Radio Buttons  (VBox-1M)
-----------------------
   Gtk.Radio_Button.Gtk_New (Radio_Right, Widget_SList.Null_List, "Rightward");
   Gtk.Radio_Button.Gtk_New (Radio_Left, Get_Group (Radio_Right), "Leftward");
   Gtk.Radio_Button.Gtk_New (Radio_Repeat, Get_Group (Radio_Right), "Repeat");
   --  Set Default = Radio_Right
   Gtk.Toggle_Button.Set_Active
     (Gtk.Toggle_Button.Gtk_Toggle_Button (Radio_Right), True);

--  Initialize Radio Button data
   Right_Data.Button_Name (1 .. 9) := "Rightward";
   Right_Data.Name_Length := 9;

   Left_Data.Button_Name (1 .. 8) := "Leftward";
   Left_Data.Name_Length := 8;

   Repeat_Data.Button_Name (1 .. 6) := "Repeat";
   Repeat_Data.Name_Length := 6;

   --  Connect radio button callbacks
   Radio_Handlers.Connect
     (Radio_Right,
      "toggled",
      On_Radio_Button_Cb'Access,
      Right_Data);

   Radio_Handlers.Connect
     (Radio_Left,
      "toggled",
      On_Radio_Button_Cb'Access,
      Left_Data);

   Radio_Handlers.Connect
     (Radio_Repeat,
      "toggled",
      On_Radio_Button_Cb'Access,
      Repeat_Data);

   Vbox_1M.Pack_Start
     (Radio_Right, Expand => False, Fill => False, Padding => 2);
   Vbox_1M.Pack_Start
     (Radio_Left,  Expand => False, Fill => False, Padding => 2);
   Vbox_1M.Pack_Start
     (Radio_Repeat,  Expand => False, Fill => False, Padding => 2);
--
------------------------
--  Entry fields  (VBox-1R)
------------------------
   Gtk_New (Entry_Grid);

   Gtk_New (Entry_L1, "Source_Speed [km/h]");
   Gtk_New (Entry_Box_Speed);
   Attach (Entry_Grid, Entry_L1, Left => 0, Top => 0, Width => 1, Height => 1);
   Attach (Entry_Grid, Entry_Box_Speed,
                                 Left => 1, Top => 0, Width => 1, Height => 1);
   Gtk.GEntry.Set_Text
     (Entry_Box_Speed,
      Ada.Strings.Fixed.Trim  --  Remove leading space from Entry box value
        (Integer'Image (Integer (S_X_Speed)), Ada.Strings.Left));
   --  Register signal handler for Enter key:"activate"
   Entry_Callback.Connect
     (Entry_Box_Speed, "activate",  --  Enter key="activate" Connection
      Entry_Callback.To_Marshaller (On_Entry_Speed_Cb'Access));
   --  Register signal handler for focus-out event
   Widget_Event_Callback.Connect
     (Entry_Box_Speed, "focus-out-event",  --  Focus-Out Connection
      Widget_Event_Callback.To_Marshaller
         (On_Speed_Focus_Out_Handler_Cb'Access));
----
   Gtk_New (Entry_L2, "Distance End-to-End [m]");
   Gtk_New (Entry_Box_Distance);
   Attach (Entry_Grid, Entry_L2, Left => 0, Top => 1, Width => 1, Height => 1);
   Attach (Entry_Grid, Entry_Box_Distance,
                                 Left => 1, Top => 1, Width => 1, Height => 1);
   Gtk.GEntry.Set_Text
     (Entry_Box_Distance,
      Ada.Strings.Fixed.Trim  --  Remove leading space from Entry box value
        (Integer'Image (Integer (S_X_Dist_M)), Ada.Strings.Left));
   --  Register signal handler for Enter key:"activate"
   Entry_Callback.Connect
     (Entry_Box_Distance, "activate",  --  Enter key="activate" Connection
      Entry_Callback.To_Marshaller (On_Entry_Distance_Cb'Access));
   --  Register signal handler for focus-out event
   Widget_Event_Callback.Connect
     (Entry_Box_Distance, "focus-out-event",  --  Focus-Out Connection
      Widget_Event_Callback.To_Marshaller
         (On_Distance_Focus_Out_Handler_Cb'Access));
----
   Gtk_New (Entry_L3, "Listner Offset [m]");
   Gtk_New (Entry_Box_Offset);
   Attach (Entry_Grid, Entry_L3, Left => 0, Top => 2, Width => 1, Height => 1);
   Attach (Entry_Grid, Entry_Box_Offset,
                                 Left => 1, Top => 2, Width => 1, Height => 1);
   Gtk.GEntry.Set_Text
     (Entry_Box_Offset,
      Ada.Strings.Fixed.Trim  --  Remove leading space from Entry box value
        (Integer'Image (Integer (L_Z_M)), Ada.Strings.Left));
   --  Register signal handler for Enter key:"activate"
   Entry_Callback.Connect
     (Entry_Box_Offset, "activate",  --  Enter key="activate" Connection
      Entry_Callback.To_Marshaller (On_Entry_Offset_Cb'Access));
   --  Register signal handler for focus-out event
   Widget_Event_Callback.Connect
     (Entry_Box_Offset, "focus-out-event",  --  Focus-Out Connection
      Widget_Event_Callback.To_Marshaller
         (On_Offset_Focus_Out_Handler_Cb'Access));
----
   Gtk_New (Entry_L4, "Source Length [m]");
   Gtk_New (Entry_Box_Slength);
   Attach (Entry_Grid, Entry_L4, Left => 0, Top => 3, Width => 1, Height => 1);
   Attach (Entry_Grid, Entry_Box_Slength,
                                 Left => 1, Top => 3, Width => 1, Height => 1);
   Gtk.GEntry.Set_Text
     (Entry_Box_Slength,
      Ada.Strings.Fixed.Trim  --  Remove leading space from Entry box value
        (Integer'Image (Integer (S_X_Len_M)), Ada.Strings.Left));
   --  Register signal handler for Enter key:"activate"
   Entry_Callback.Connect
     (Entry_Box_Slength, "activate",  --  Enter key="activate" Connection
      Entry_Callback.To_Marshaller (On_Entry_Slength_Cb'Access));
   --  Register signal handler for focus-out event
   Widget_Event_Callback.Connect
     (Entry_Box_Slength, "focus-out-event",  --  Focus-Out Connection
      Widget_Event_Callback.To_Marshaller
        (On_Slength_Focus_Out_Handler_Cb'Access));

----
   Gtk_New (Entry_L5, "Source Count [pcs]");
   Gtk_New (Entry_Box_SourceCount);
   Attach (Entry_Grid, Entry_L5, Left => 0, Top => 4, Width => 1, Height => 1);
   Attach (Entry_Grid, Entry_Box_SourceCount,
                                 Left => 1, Top => 4, Width => 1, Height => 1);
   Gtk.GEntry.Set_Text
     (Entry_Box_SourceCount,
      Ada.Strings.Fixed.Trim  --  Remove leading space from Entry box value
        (Integer'Image (Integer (S_Count)), Ada.Strings.Left));
   --  Register signal handler for Enter key:"activate"
   Entry_Callback.Connect
     (Entry_Box_SourceCount, "activate",  --  Enter key="activate" Connection
      Entry_Callback.To_Marshaller (On_Entry_SourceCount_Cb'Access));
   --  Register signal handler for focus-out event
   Widget_Event_Callback.Connect
     (Entry_Box_SourceCount, "focus-out-event",  --  Focus-Out Connection
      Widget_Event_Callback.To_Marshaller
        (On_SourceCount_Focus_Out_Handler_Cb'Access));

   Vbox_1R.Add (Entry_Grid);
--
----------------------------
--  Scale Volume Slider  (VBox-1M)
----------------------------
------------
--  SOURCE Gain
------------
   Gtk_New (Label_S_Vol, "Source Gain");
   Vbox_1M.Pack_Start
     (Label_S_Vol, Expand => False, Fill => False, Padding => 2);

   --  Set slider range in logarithmic space
   Gtk.Scale.Gtk_New_Hscale
     (Scale => Scale_S_Gain_Slider,
      Min => Glib.Gdouble (-20.0),  --  10^(-20/10) = approx 0.01
      Max => Glib.Gdouble (20.0),   --  10^(10/10) = 10.0
      Step => Glib.Gdouble (1.0));

--  Default value: 0.0 dB = 1.0x
   Gtk.Scale.Set_Value (Scale_S_Gain_Slider, 0.0);  --  Initialize 0 dB

-- Add marks with dB display
   Gtk.Scale.Add_Mark
     (Scale_S_Gain_Slider, -20.0, Gtk.Enums.Pos_Top, "-20dB(0.1x)");
   Gtk.Scale.Add_Mark
     (Scale_S_Gain_Slider, -6.0, Gtk.Enums.Pos_Top, "-6dB");
   Gtk.Scale.Add_Mark
     (Scale_S_Gain_Slider, 0.0, Gtk.Enums.Pos_Top, "0dB(1x)");
   Gtk.Scale.Add_Mark
     (Scale_S_Gain_Slider, 6.0, Gtk.Enums.Pos_Top, "6dB");
   Gtk.Scale.Add_Mark
     (Scale_S_Gain_Slider, 20.0, Gtk.Enums.Pos_Top, "20dB(10x)");
--
   Vbox_1M.Pack_Start
     (Scale_S_Gain_Slider, Expand => False, Fill => False, Padding => 0);

   Scale_Volume_Callback.Connect
     (Scale_S_Gain_Slider, "value-changed",
      Scale_Volume_Callback.To_Marshaller
        (Process.On_S_Scale_Volume_Cb'Access));
   --
   S_Gain_DB := Gtk.Scale.Get_Value (Scale_S_Gain_Slider);
   S_Gain_Linear := Gdouble (10.0 ** Float (S_Gain_DB / 20.0));
--
------------
--  LISTENER Gain
------------
   Gtk_New (Label_L_Vol, "Listener Gain");
   Vbox_1M.Pack_Start
     (Label_L_Vol, Expand => False, Fill => False, Padding => 2);

   --  Set slider range in logarithmic space
   Gtk.Scale.Gtk_New_Hscale
     (Scale => Scale_L_Gain_Slider,
      Min => Glib.Gdouble (-20.0),  --  10^(-20/10) = approx 0.01
      Max => Glib.Gdouble (20.0),   --  10^(10/10) = 10.0
      Step => Glib.Gdouble (1.0));

--  Default value: 0.0 dB = 1.0x
   Gtk.Scale.Set_Value (Scale_L_Gain_Slider, 0.0);  --  Initialize 0 dB

-- Add marks with dB display
   Gtk.Scale.Add_Mark
     (Scale_L_Gain_Slider, -20.0, Gtk.Enums.Pos_Top, "-20dB(0.1x)");
   Gtk.Scale.Add_Mark
     (Scale_L_Gain_Slider, -6.0, Gtk.Enums.Pos_Top, "-6dB");
   Gtk.Scale.Add_Mark
     (Scale_L_Gain_Slider, 0.0, Gtk.Enums.Pos_Top, "0dB(1x)");
   Gtk.Scale.Add_Mark
     (Scale_L_Gain_Slider, 6.0, Gtk.Enums.Pos_Top, "6dB");
   Gtk.Scale.Add_Mark
     (Scale_L_Gain_Slider, 20.0, Gtk.Enums.Pos_Top, "20dB(10x)");
--
   Vbox_1M.Pack_Start
     (Scale_L_Gain_Slider, Expand => False, Fill => False, Padding => 0);

   Scale_Volume_Callback.Connect
     (Scale_L_Gain_Slider, "value-changed",
      Scale_Volume_Callback.To_Marshaller
        (Process.On_L_Scale_Volume_Cb'Access));
   --
   L_Gain_DB := Gtk.Scale.Get_Value (Scale_L_Gain_Slider);
   L_Gain_Linear := Gdouble (10.0 ** Float (L_Gain_DB / 20.0));
--
--
--------------------------
--  Create and setup drawing area
--------------------------
   Gtk.Drawing_Area.Gtk_New (Canvas);
   C_Drawing_Area := Canvas;       --  Initialize animation data
   Canvas.Set_Size_Request (1000, 200);
   Vbox_3.Add (Canvas);
   --  Connect drawing event
   Draw_Cb.Connect (Canvas, "draw", Draw_Cb.To_Marshaller (On_Draw'Access));
--
--------------------------
--  Start Timer Interrupt
--------------------------
   Gid_Dummy := Time_Cb.Timeout_Add
     (T_Intr_Period, Timer_Intr'Access, Dummy_Int);
--
   Process_Initialize;
--
   Win.On_Delete_Event (Delete_Handler'Access, Win);
--  Show everything
   Win.Show_All;

   Gtk.Main.Main;

end Gui_Doppler_Linear_m;
