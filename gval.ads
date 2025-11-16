--  This file is GLOBAL VALIABLES  (.ADS only)
with OpenAL.Source;  --  use OpenAL.Source;
with OpenAL.Buffer;    use OpenAL.Buffer;
with OpenAL.Context; --  use OpenAL.Context;
with OpenAL.Types;     use OpenAL.Types;
with Interfaces;       use Interfaces;
with Gtk.Drawing_Area;
with Ada.Numerics.Elementary_Functions;
use Ada.Numerics.Elementary_Functions;
with Gtk.Handlers;   --  use Gtk.Handlers;
with Ada.Strings.Unbounded;  use Ada.Strings.Unbounded;  --  For File name
with OpenAL.Error;  --  use OpenAL.Error;
with Glib;             use Glib;
package Gval is
--
----------------------------------------
--  GUI
----------------------------------------
--
--  Packages for event handling
   package Draw_Cb is new Gtk.Handlers.Return_Callback
     (Gtk.Drawing_Area.Gtk_Drawing_Area_Record, Boolean);
--
   Canvas     : Gtk.Drawing_Area.Gtk_Drawing_Area;
--
   T_Intr_Period : constant := 10; --  Timer Intr Cycle 10=10msec, 50=50msec
--  For Draw parameters ------
   C_Drawing_Area : Gtk.Drawing_Area.Gtk_Drawing_Area;
   S_X_Space : constant := 100.0;  --  Canvas space pixel
   S_Y_Pix   : constant := 70.0;   --  Source Y Location pixel
   S_X_Dist_M : Float := 200.0;    --  Setting L-R.End to End meter Entry_Dista
   S_X_Dist_Pix : constant := 800.0; --  Distance Canvas L.End-R.End pixel
   S_Radius  : constant := 10.0;   --  Source Round Radius pixel
   S_X_Len_M : Float := 40.0;       --  Source Length meter
   S_X_Len_Pix : Float := S_X_Len_M * S_X_Dist_Pix / S_X_Dist_M;
   L_X_Pix   : constant := 500.0;  --  Listner X Location pixel
   L_X_M     : Float := (L_X_Pix - S_X_Space) * S_X_Dist_M / S_X_Dist_Pix;
   L_Z_M     : Float := 2.0;       --  Listner Z Offset meter Entry_Offset
   L_Z_Pix_Factor : constant := 10.0;  --  Z display factor
   L_Z_Pix   : Float := S_Y_Pix + L_Z_Pix_Factor * Sqrt (L_Z_M);
   L_Radius  : constant := 8.0;    --  Listner Round Radius pixel
--------------
   Canvas_Width : constant := 1000.0;  --  pixel
   Canvas_Height : constant := 200.0;  --  pixel
   S_X_Speed : Float := 100.0;         --  Setting Value km/s  Entry_Speed
   S_X_Pos_M : Float;        --  Calculation meter, Current position from 0
   S_X_Pos_Pix : Float;      --  Calculation pixel, Current position from 100
   S_X_Dist_M_Intr : Float;  --  Calculation per Step, meter/Tintr
   Progress  : Float := 0.0;  --  Calculation ratio
--
--
   type Radio_Selection is (Rightward, Leftward, Repeating);
   Current_Radio_Selected : Radio_Selection;
--
   type Direction is (Right, Left);
   Source_Direction : Direction := Right;
--
   type Process_Name is  --  For Process_State;
     (Standby,
      Start_Right, Busy_Right, Pause_Right,
      Start_Left,  Busy_Left,  Pause_Left);
   Process_State : Process_Name := Standby;
--
--
   Target_X : Float := 0.0;
   Current_X : Float := 0.0;
   Radio_Right_Flag   : Boolean := True;
   Radio_Left_Flag    : Boolean := False;
   Radio_Repeat_Flag  : Boolean := False;

   Entry_Speed : Float    := S_X_Speed;   --  km/h
   Entry_Distance : Float := S_X_Dist_M;  --  m
   Entry_Offset : Float   := L_Z_M;     --  m
--
--
--
--
-----------------------------------------------
--  Audio
-----------------------------------------------
--  WAV file structure
   type WAV_Header is record
      ChunkID       : String (1 .. 4);   --  "RIFF"
      ChunkSize     : Unsigned_32;       --  This Byte# + 8 = File size Byte#
      Format        : String (1 .. 4);   --  "WAVE" = 0x57415645
      Subchunk1ID   : String (1 .. 4);   --  "fmt " = 0x666D7420
      Subchunk1Size : Unsigned_32;       --  Linear PCM = 16 = 0x10000000
      AudioFormat   : Unsigned_16;  --  No compressed Linear PCM = 1 = 0x0100
      NumChannels   : Unsigned_16;  --  Mono = 1 (0x0100), 2CH = 2 = 0x200
      SampleRate    : Unsigned_32;  --  Sample rate 44.1k = 44100 = 0x44AC0000
      ByteRate      : Unsigned_32;  --  Byte/sec 16b/44.1k/Mono = 88200
      BlockAlign    : Unsigned_16;  --  Byte/Sample 16b/Mono = 2
      BitsPerSample : Unsigned_16;  --  16bit = 16
      Subchunk2ID   : String (1 .. 4);  --  "data" = 0x64617461
      Subchunk2Size : Unsigned_32;  --  Nubmer of Data Byte
   end record;
--
--  16bit for Audio data
   type Sample_16_t_Array_Access is access all Sample_Array_16_t;
--  16bit WAV store type
   type WAV_Data_Record is record
      Header : WAV_Header;
      Data   : Sample_16_t_Array_Access;
      Data_W_Count : Natural;           -- Number of 16-bit Word Samples
   end record;
   WAV_File : WAV_Data_Record;  --  Dynamic Allocation
   WAV_Byte_Length : Integer;
   --
   UB_Sound_File_Name : Unbounded_String := To_Unbounded_String ("");
   UB_Chooser_Filename_Full : Unbounded_String :=
      To_Unbounded_String ("./testsound.wav");
   UB_Chooser_Filename_Only : Unbounded_String :=
      To_Unbounded_String ("testsound.wav");
   --
   Buffer_ID_SN : Unsigned_Integer_t;
   Device : OpenAL.Context.Device_t;
   Context : OpenAL.Context.Context_t;
   CX_Devicet : OpenAL.Context.Device_t;
   Set_Active_Context : Boolean;
   EndFlag : Integer := 0;
   ProcessedNr : Natural := 0;
   OpenAL_Errort : OpenAL.Error.Error_t;
   Audio_Dead_Zone : Boolean := False;
   --
   ROLLOFF_FACTOR : constant Float := 1.0;
   S_Gain_Linear : Glib.Gdouble;  --  Source Linear Gain Linear
   S_Gain_DB : Glib.Gdouble;      --  Source Gain dB
--   SOURCE_GAIN : constant Float := 1.0;  --  1.=0dB Source Volume is constant
--   Source_Deadzone_Factor: constant Float := 1.3; --  Source Deadzone factor
--  Location X, Y, Z    3 Dimensions
                                  --
   S_Count  : Float := 10.0;         --  How many Sources on the train
   S_COUNT_MAX : constant Natural := 10;
   Source_Set_Position : OpenAL.Types.Vector_3f_t := (-1000.0, 0.0, 0.0);
   Source_Set_Velocity : OpenAL.Types.Vector_3f_t := (0.0, 0.0, 0.0);
--   Source_Set_Direction : OpenAL.Types.Vector_3f_t := (1.0, 0.0, 0.0);

--
--   Sound_Source : OpenAL.Source.Source_t;
--   Sound_Source : OpenAL.Source.Source_t (1 .. S_COUNT_MAXS
--  type Source_Type_Index is range 1 .. S_COUNT_MAX;
--  type Sound_Source_t is array (Source_Type_Index) of OpenAL.Source.Source_t;
--  Sound_Source : Sound_Source_t;

   Listener_Set_Position : constant OpenAL.Types.Vector_3f_t :=
     (0.0, 0.0, Float_t (-L_Z_M));
   Listener_Set_Velocity : constant OpenAL.Types.Vector_3f_t :=
     (0.0, 0.0, 0.0);
   Listener_Orientation_Forward : constant OpenAL.Types.Vector_3f_t :=
     (0.0, 0.0, -1.0);  --  Front: Z= -1 Minus
   Listener_Orientation_Up : constant OpenAL.Types.Vector_3f_t :=
     (0.0, 1.0, 0.0);   --  Normal Y=+1
   L_Gain_Linear : Glib.Gdouble;  --  Listener Gain Linear
   L_Gain_DB : Glib.Gdouble;      --  Source Gain dB
--
   Sound_Source_Array : OpenAL.Source.Source_Array_t (1 .. S_COUNT_MAX);
--   Sound_Source_Array : OpenAL.Source.Source_Array_t (1 .. 1);
--
   WAV_Buffers : OpenAL.Buffer.Buffer_Array_t (1 .. S_COUNT_MAX);
--
   SetPosF, SetVelF : OpenAL.Types.Float_t;   --  Vector Calculation
--
end Gval;
