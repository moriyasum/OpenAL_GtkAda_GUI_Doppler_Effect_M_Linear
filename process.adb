
with Gval;            use Gval;
with Gui;             use Gui;
with Audio;           use Audio;
with Ada.Text_IO;     use Ada.Text_IO;
with Ada.Text_IO.Unbounded_IO; use Ada.Text_IO.Unbounded_IO;
with Ada.Strings.Unbounded;  use Ada.Strings.Unbounded;  --  For Filename
with Ada.Strings.Fixed;  --  use Ada.Strings.Fixed;
with Gtk.Toggle_Button; use Gtk.Toggle_Button;
with Gtk.Main;
with Ada.Numerics.Elementary_Functions;
use Ada.Numerics.Elementary_Functions;
with Glib;            use Glib;
with Gtk.File_Chooser_Button;  use Gtk.File_Chooser_Button;
with Ada.Directories; use Ada.Directories;  --  For Simple Filename
with OpenAL.Types;    use OpenAL.Types;
with Gtk.Scale;       use Gtk.Scale;
with OpenAL.Source;   use OpenAL.Source;
with OpenAL.Listener; use OpenAL.Listener;

package body Process is

--------------------------------------------------------
--------------------------------------------------------
--  BUTTON CALLBACKS
--------------------------------------------------------
--------------------------------------------------------
--
   function Delete_Handler
     (Self  : access Glib.Object.GObject_Record'Class;
      Event : Gdk_Event) return Boolean is
   pragma Unreferenced (Self);
   pragma Unreferenced (Event);
   begin
      Gtk.Main.Main_Quit;
      return False;  -- False : CLOSING
   end Delete_Handler;
--
--
   procedure On_Button_Play_Cb
      (Button : access Gtk_Button_Record'Class;
       Data  : Callback_Data_Access) is
      pragma Unreferenced (Button);
   begin
      Put_Line ("Play pressed " & Integer'Image (Data.Number));
         --  Standby or Busy then from scratch
--         TimeCnt := 0;
         if Process_State = Standby then
            if Current_Radio_Selected = Rightward then    --  Radio=Right
               Process_State := Start_Right;
               Source_Direction := Right;
            elsif Current_Radio_Selected = Leftward then  --  Radio=Left
               Process_State := Start_Left;
               Source_Direction := Left;
            else                                          --  Radio=Repeat
               Process_State := Start_Right;  --  Repeat begins Right
               Source_Direction := Right;
            end if;
         elsif Process_State = Pause_Right then  --  "Pause Right mode"
            Process_State := Busy_Right;   --  Continue Playing
         elsif Process_State = Pause_Left then  --  "Pause Left mode"
            Process_State := Busy_Left;   --  Continue Playing
         else
            null;
         end if;
   end On_Button_Play_Cb;
   --
   --
   procedure On_Button_Pause_Cb
      (Button : access Gtk_Button_Record'Class;
       Data  : Callback_Data_Access) is
      pragma Unreferenced (Button);
      S_Gain : OpenAL.Types.Float_t;
      L_Gain : OpenAL.Types.Float_t;
   begin
      Put_Line ("Pause pressed" & Integer'Image (Data.Number));
      if Process_State = Busy_Right then     --  "Pause Right mode"
         Process_State := Pause_Right;
      elsif Process_State = Busy_Left then   --  "Pause Left mode"
         Process_State := Pause_Left;
      end if;
--
--  For debug
--  S_X_Pos_Pix := Progress * S_X_Dist_Pix + S_X_Space;
      S_X_Dist_M_Intr :=
        Float (T_Intr_Period) / 1000.0 * S_X_Speed * 1000.0 / 3600.0;
      Put_Line ("Process_State=" & Process_Name'Image (Process_State));
      Put_Line ("Progress=" & Float'Image (Progress));
      Put_Line ("S_X_Dist_M_Intr=" & Float'Image (S_X_Dist_M_Intr));
      Put_Line ("Source Length[m]=" & Float'Image (S_X_Len_M));
      Put_Line ("S_X_Pos_Pix=" & Float'Image (S_X_Pos_Pix));
      Put_Line ("S_X_POS_M[m]=" & Float'Image (S_X_Pos_M));
      Put_Line ("L_X_M [m]=" & Float'Image (L_X_M));
      Put_Line ("X-Listner[m]=" & Float'Image (S_X_Pos_M - L_X_M));
      Put_Line ("Set Audio X : SetPosF [m]=" &
                  OpenAL.Types.Float_t'Image (SetPosF));
--

      OpenAL.Source.Get_Gain (Sound_Source_Array (1), S_Gain);  --  ################
      OpenAL.Listener.Get_Gain (L_Gain);
      Put_Line ("Source Linear_Gain=" & Float_t'Image (S_Gain) &
                ",   Listener Linear_Gain=" & Float_t'Image (L_Gain));

--
   end On_Button_Pause_Cb;
--
--
   procedure On_Button_Stop_Cb
      (Button : access Gtk_Button_Record'Class;
       Data  : Callback_Data_Access) is
      pragma Unreferenced (Button);
   begin
      Put_Line ("Stop pressed" & Integer'Image (Data.Number));
      if Process_State /= Standby then
         Process_State := Standby;
         --  Audio_End;  --  Do not use this sub.
         Audio_Stop;
         Gui_End;
      end if;
   end On_Button_Stop_Cb;
--
--
   procedure On_File_Chooser_Cb
     (Widget : access Gtk.Widget.Gtk_Widget_Record'Class) is

      Chooser_Widget :
        constant Gtk.File_Chooser_Button.Gtk_File_Chooser_Button :=
           Gtk.File_Chooser_Button.Gtk_File_Chooser_Button (Widget);

   begin
      declare
         Selected_Filename : constant UTF8_String :=
           Gtk.File_Chooser_Button.Get_Filename (Chooser_Widget);
      begin
         if Selected_Filename /= "" then
--            Put_Line ("Full path filename: " & Selected_Filename);
            UB_Chooser_Filename_Full :=
               To_Unbounded_String (Selected_Filename);
--            UB_Chooser_Filename_Only := Simple_Name (Selected_Filename);
            UB_Chooser_Filename_Only :=
               To_Unbounded_String (Simple_Name (Selected_Filename));
            Put_Line ("Full path filename: " & UB_Chooser_Filename_Full);
            Put_Line ("Filename only :  " & UB_Chooser_Filename_Only);
         else
            Put_Line ("No file selected");
         end if;
      end;

   end On_File_Chooser_Cb;
--
--
   procedure On_Radio_Button_Cb
     (Widget : access Gtk_Widget_Record'Class;
      Data   : Radio_Data_Access) is
   begin
      --  Check if the radio button is active (selected)
      if Get_Active (Gtk_Toggle_Button (Widget)) then
         if Data.Button_Name (1 .. Data.Name_Length) = "Rightward" then
            Current_Radio_Selected := Rightward;
         elsif Data.Button_Name (1 .. Data.Name_Length) = "Leftward" then
            Current_Radio_Selected := Leftward;
         else
            Current_Radio_Selected := Repeating;
         end if;
         Put_Line ("Radio button selected: " &
                   Data.Button_Name (1 .. Data.Name_Length));
         if Current_Radio_Selected = Repeating then
            Put_Line ("Just checking Repeating");
         end if;
      end if;
   end On_Radio_Button_Cb;
--
--
-------------------
--  Entry Speed Callback
-------------------
   procedure On_Entry_Speed_Cb
     (Widget : access Gtk.GEntry.Gtk_Entry_Record'Class) is
      Entry_Widget : constant Gtk.GEntry.Gtk_Entry :=
         Gtk.GEntry.Gtk_Entry (Widget);
      Text_Value   : constant String := Gtk.GEntry.Get_Text (Entry_Widget);
   begin
      declare
      begin
         Entry_Speed := Float'Value (Text_Value);
         S_X_Speed := Entry_Speed;
         Put_Line ("Speed = " & Float'Image (Entry_Speed));

      exception
         when Constraint_Error =>
            Gtk.GEntry.Set_Text         --  Wrong text sets previous value
--               (Entry_Box_Speed,        --  Display Value
               (Entry_Widget,
                Ada.Strings.Fixed.Trim  --  Remove leading space from Entry
                  (Integer'Image (Integer (S_X_Speed)), Ada.Strings.Left));
            Ada.Text_IO.Put_Line ("Please enter a valid number");
      end;
   end On_Entry_Speed_Cb;
--
   function On_Speed_Focus_Out_Handler_Cb
     (Widget : access Gtk.Widget.Gtk_Widget_Record'Class;
      Event  : Gdk.Event.Gdk_Event) return Boolean is
      pragma Unreferenced (Event);
      Entry_Widget : constant Gtk.GEntry.Gtk_Entry :=
         Gtk.GEntry.Gtk_Entry (Widget);
   begin
      On_Entry_Speed_Cb (Entry_Widget);
      return False;
   end On_Speed_Focus_Out_Handler_Cb;
--
--
-------------------
--  Entry Distance Callback
-------------------
   procedure On_Entry_Distance_Cb
     (Widget : access Gtk.GEntry.Gtk_Entry_Record'Class) is
      Entry_Widget : constant Gtk.GEntry.Gtk_Entry :=
        Gtk.GEntry.Gtk_Entry (Widget);
      Text_Value   : constant String := Gtk.GEntry.Get_Text (Entry_Widget);
   begin
      declare
      begin
         Entry_Distance := Float'Value (Text_Value);
         S_X_Dist_M := Entry_Distance;
         Put_Line ("Distance = " & Float'Image (Entry_Distance));
         --  Update Listner Location [meter]
         L_X_M := (L_X_Pix - S_X_Space) * S_X_Dist_M / S_X_Dist_Pix;
         S_X_Len_Pix := S_X_Len_M * S_X_Dist_Pix / S_X_Dist_M;
      exception
         when Constraint_Error =>
            Gtk.GEntry.Set_Text         --  Wrong text sets previous value
               (Entry_Widget,     --  Display Value
                Ada.Strings.Fixed.Trim  --  Remove leading space from Entry
                  (Integer'Image (Integer (S_X_Dist_M)), Ada.Strings.Left));
            Ada.Text_IO.Put_Line ("Please enter a valid number");
      end;
   end On_Entry_Distance_Cb;
--
   function On_Distance_Focus_Out_Handler_Cb
     (Widget : access Gtk.Widget.Gtk_Widget_Record'Class;
      Event  : Gdk.Event.Gdk_Event) return Boolean is
      pragma Unreferenced (Event);
      Entry_Widget : constant Gtk.GEntry.Gtk_Entry :=
         Gtk.GEntry.Gtk_Entry (Widget);
   begin
      On_Entry_Distance_Cb (Entry_Widget);
      return False;
   end On_Distance_Focus_Out_Handler_Cb;
--
--
-------------------
--  Entry Offset Callback
-------------------
   procedure On_Entry_Offset_Cb
     (Widget : access Gtk.GEntry.Gtk_Entry_Record'Class) is
      Entry_Widget : constant Gtk.GEntry.Gtk_Entry :=
        Gtk.GEntry.Gtk_Entry (Widget);
      Text_Value   : constant String := Gtk.GEntry.Get_Text (Entry_Widget);
   begin
      declare
      begin
         Entry_Offset := Float'Value (Text_Value);
         L_Z_M := Entry_Offset;
         L_Z_Pix := S_Y_Pix + L_Z_Pix_Factor * Sqrt (L_Z_M);
         Put_Line ("Offset = " & Float'Image (Entry_Offset));

      exception
         when Constraint_Error =>
            Gtk.GEntry.Set_Text         --  Wrong text sets previous value
               (Entry_Widget,       --  Display Value
                Ada.Strings.Fixed.Trim  --  Remove leading space from Entry
                  (Integer'Image (Integer (L_Z_M)), Ada.Strings.Left));
            Ada.Text_IO.Put_Line ("Please enter a valid number");
      end;
   end On_Entry_Offset_Cb;
   --
   function On_Offset_Focus_Out_Handler_Cb
     (Widget : access Gtk.Widget.Gtk_Widget_Record'Class;
      Event  : Gdk.Event.Gdk_Event) return Boolean is
      pragma Unreferenced (Event);
      Entry_Widget : constant Gtk.GEntry.Gtk_Entry :=
         Gtk.GEntry.Gtk_Entry (Widget);
   begin
      On_Entry_Offset_Cb (Entry_Widget);
      return False;
   end On_Offset_Focus_Out_Handler_Cb;
--
--
-------------------
--  Entry Source-Length Callback
-------------------
   procedure On_Entry_Slength_Cb
     (Widget : access Gtk.GEntry.Gtk_Entry_Record'Class) is
      Entry_Widget : constant Gtk.GEntry.Gtk_Entry :=
        Gtk.GEntry.Gtk_Entry (Widget);
      Text_Value   : constant String := Gtk.GEntry.Get_Text (Entry_Widget);
   begin
      declare
      begin
         S_X_Len_M := Float'Value (Text_Value);
--         if S_X_Len_M < 0.1 then
--            S_X_Len_M := 0.1;
--            Gtk.GEntry.Set_Text   --  Less than 0.5 then sets Minimum 0.5m
--               (Entry_Widget,     --  Display Value
--                Ada.Strings.Fixed.Trim  --  Remove leading space from Entry
--                  ("0.1", Ada.Strings.Left));
--         end if;
         S_X_Len_Pix := S_X_Len_M * S_X_Dist_Pix / S_X_Dist_M;
         if S_X_Len_Pix = 0.0 then
            S_X_Len_Pix := 1.0;
         end if;
         Put_Line ("Source Length = " & Float'Image (S_X_Len_M));

      exception
         when Constraint_Error =>
            Gtk.GEntry.Set_Text         --  Wrong text sets previous value
               (Entry_Widget,       --  Display Value
                Ada.Strings.Fixed.Trim  --  Remove leading space from Entry
                  (Integer'Image (Integer (S_X_Len_M)), Ada.Strings.Left));
            Ada.Text_IO.Put_Line ("Please enter a valid number");
      end;
   end On_Entry_Slength_Cb;
   --
   function On_Slength_Focus_Out_Handler_Cb
     (Widget : access Gtk.Widget.Gtk_Widget_Record'Class;
      Event  : Gdk.Event.Gdk_Event) return Boolean is
      pragma Unreferenced (Event);
      Entry_Widget : constant Gtk.GEntry.Gtk_Entry :=
         Gtk.GEntry.Gtk_Entry (Widget);
   begin
      On_Entry_Slength_Cb (Entry_Widget);  --  Call above "Enter" procedure
      return False;
   end On_Slength_Focus_Out_Handler_Cb;
--
--
--
--
-------------------
--  Entry Number_of_Sources Callback
-------------------
   procedure On_Entry_SourceCount_Cb
     (Widget : access Gtk.GEntry.Gtk_Entry_Record'Class) is
      Entry_Widget : constant Gtk.GEntry.Gtk_Entry :=
        Gtk.GEntry.Gtk_Entry (Widget);
      Text_Value   : constant String := Gtk.GEntry.Get_Text (Entry_Widget);
   begin
      declare
      begin
         S_Count := Float'Value (Text_Value);
         if S_Count > 10.0 then
            S_Count := 10.0;
            Gtk.GEntry.Set_Text   --  More than 10 then sets Max 10
              (Entry_Widget,     --  Display Value
               Ada.Strings.Fixed.Trim  --  Remove leading space from Entry
                  ("10", Ada.Strings.Left));
         end if;

         Put_Line ("Source Count = " & Float'Image (S_Count));

      exception
         when Constraint_Error =>
            Gtk.GEntry.Set_Text         --  Wrong text sets previous value
               (Entry_Widget,       --  Display Value
                Ada.Strings.Fixed.Trim  --  Remove leading space from Entry
                  (Integer'Image (Integer (S_Count)), Ada.Strings.Left));
            Ada.Text_IO.Put_Line ("Please enter a valid number");
      end;
   end On_Entry_SourceCount_Cb;
   --
   function On_SourceCount_Focus_Out_Handler_Cb
     (Widget : access Gtk.Widget.Gtk_Widget_Record'Class;
      Event  : Gdk.Event.Gdk_Event) return Boolean is
      pragma Unreferenced (Event);
      Entry_Widget : constant Gtk.GEntry.Gtk_Entry :=
         Gtk.GEntry.Gtk_Entry (Widget);
   begin
      On_Entry_SourceCount_Cb (Entry_Widget);  --  Call above "Enter" procedure
      return False;
   end On_SourceCount_Focus_Out_Handler_Cb;
--
--
--
------------------
--  Gain Volume Slider Callback
------------------
--  SOURCE
   procedure On_S_Scale_Volume_Cb
     (Widget : access Gtk.Widget.Gtk_Widget_Record'Class) is
      Scale_Widget : constant Gtk.Scale.Gtk_Hscale :=
        Gtk.Scale.Gtk_Hscale (Widget);
   begin
--      S_Gain_Linear := Gtk.Scale.Get_Value (Scale_Widget);
      S_Gain_DB := Gtk.Scale.Get_Value (Scale_Widget);
      S_Gain_Linear := Gdouble (10.0 ** Float (S_Gain_DB / 20.0));
         Put_Line ("Source Volume: " & Glib.Gdouble'Image (S_Gain_Linear));
      Audio_Set_Volumes;

   end On_S_Scale_Volume_Cb;
--
--  LISTENER
   procedure On_L_Scale_Volume_Cb
     (Widget : access Gtk.Widget.Gtk_Widget_Record'Class) is
      Scale_Widget : constant Gtk.Scale.Gtk_Hscale :=
        Gtk.Scale.Gtk_Hscale (Widget);
   begin
--      L_Gain_Linear := Gtk.Scale.Get_Value (Scale_Widget);
      L_Gain_DB := Gtk.Scale.Get_Value (Scale_Widget);
      L_Gain_Linear := Gdouble (10.0 ** Float (L_Gain_DB / 20.0));
         Put_Line ("Listener Volume: " & Glib.Gdouble'Image (L_Gain_Linear));
      Audio_Set_Volumes;

   end On_L_Scale_Volume_Cb;
--
--
--
--------------------------------------------------------
--------------------------------------------------------
--  TIMER INTERRUPT, PROCESS SEQUENCER
--------------------------------------------------------
--------------------------------------------------------
   function Timer_Intr (Dummy_Int : Integer) return Boolean is
      pragma Unreferenced (Dummy_Int);
   begin
      TimerIntrCounter := TimerIntrCounter + 1;
      case Process_State is
         when Standby =>         --  Standby, just waiting
            null;
--
         when Start_Right =>         --  Begin Start Right was pressed
            Audio_Initialize_Open;
            Audio_Start;
            Gui_Initialize_Left_End;
            Process_State := Busy_Right;
            Put_Line ("In Tintr Start Right");

         when Busy_Right =>
         --  Get Current Processed Percentage
            S_X_Dist_M_Intr :=
            Float (T_Intr_Period) / 1000.0 * S_X_Speed * 1000.0 / 3600.0;

            if Progress < 1.0  then

               S_X_Pos_M := S_X_Pos_M + S_X_Dist_M_Intr;

               Progress := S_X_Pos_M / S_X_Dist_M;

               S_X_Pos_Pix := Progress * S_X_Dist_Pix + S_X_Space;

            else   --  Progress >= 1,0 Ritht End
               if Current_Radio_Selected = Repeating then
                  Gui_Initialize_Right_End;
                  Process_State := Busy_Left;
                  Source_Direction := Left;
               else  --  Stopped at Right End
                  Progress := 1.0;
                  Process_State := Standby;
                  Audio_Stop;
               end if;
            end if;
--
--
         when Start_Left =>         --  Begin Start Right was pressed
            Audio_Initialize_Open;
            Audio_Start;
            Gui_Initialize_Right_End;
            Process_State := Busy_Left;
--
         when Busy_Left =>
         --  Get Current Processed Percentage
            S_X_Dist_M_Intr :=
            Float (T_Intr_Period) / 1000.0 * S_X_Speed * 1000.0 / 3600.0;

            if Progress < 1.0  then

               S_X_Pos_M := S_X_Pos_M - S_X_Dist_M_Intr;

               Progress := (S_X_Dist_M - S_X_Pos_M) / S_X_Dist_M;

               S_X_Pos_Pix :=
                 S_X_Dist_Pix + S_X_Space - (Progress * S_X_Dist_Pix);

            else   --  Progress >= 1,0 Left End
               if Current_Radio_Selected = Repeating then
                  Gui_Initialize_Left_End;
                  Process_State := Busy_Right;
                  Source_Direction := Right;
               else     --  Stopped at Left End
                  Progress := 1.0;
                  Process_State := Standby;
                  Audio_Stop;
               end if;
            end if;

         when Pause_Left | Pause_Right =>
            null;
      end case;
--
      C_Drawing_Area.Queue_Draw;        --  Request Re-Draw

      if S_Count = 1.0 then
         Audio_Playing_Parameters_1;  --  Single Source processing
      else
         Audio_Playing_Parameters_m;  --  Multi Sources processing
      end if;
--
      return True;   --  True=Continue, False=Stop, no more T_Intr
   end Timer_Intr;
--
--
---------------------------------------
--  Initilize
---------------------------------------
   procedure Process_Initialize is
   begin
      Gui_Initialize_Left_End;

--      Audio_Initialize_Open;  --  Audio file open, all OpenAL setup

   end Process_Initialize;
--
end Process;
