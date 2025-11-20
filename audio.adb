
--  with Gval;                  use Gval;
with Ada.Text_IO;           use Ada.Text_IO;
with OpenAL.Context;        use OpenAL.Context;
with OpenAL.Listener;     --  use OpenAL.Listener;
with Ada.Unchecked_Deallocation;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;  --  File name
with OpenAL.Global;     --    use OpenAL.Global;  --  for Doppler Factor
with OpenAL.Source;
with Ada.Streams.Stream_IO; use Ada.Streams.Stream_IO;
with Interfaces;      use Interfaces;
with OpenAL.Buffer;   use OpenAL.Buffer;
with Ada.Streams;           use Ada.Streams;
with Glib;            use Glib;
with OpenAL.Error;    use OpenAL.Error;
with OpenAL.Types;    use OpenAL.Types;
--
package body Audio is
--
   procedure Free is new Ada.Unchecked_Deallocation
     (Sample_Array_16_t, Sample_16_t_Array_Access);
   --
----------------------------------------------------
----------------------------------------------------
--  Read WAV file
----------------------------------------------------
----------------------------------------------------

   BinF            : Ada.Streams.Stream_IO.File_Type;
   Header          : WAV_Header;
   Data            : Sample_16_t_Array_Access;
   Data_B_Count    : Natural := 0;    -- Data size in Bytes
   Data_W_Count_t  : Sample_Size_t := 1; -- Number of 16bit samples  Minimum=1
   Result          : WAV_Data_Record;

--
   function Load_WAV_File (File_Name : String) return WAV_Data_Record is

      Chunk_ID        : String (1 .. 4);
      Chunk_Size      : Unsigned_32;
      Fmt_Found       : Boolean := False;
      Data_Found      : Boolean := False;

   --  Read 4 characters from file
      function Read_Chunk_ID
        (BinF : in out Ada.Streams.Stream_IO.File_Type) return String is
         Buffer : Ada.Streams.Stream_Element_Array (1 .. 4);
         Last : Ada.Streams.Stream_Element_Offset;
      begin
         Ada.Streams.Stream_IO.Read (BinF, Buffer, Last);
         return Character'Val (Buffer (1)) &
                Character'Val (Buffer (2)) &
                Character'Val (Buffer (3)) &
                Character'Val (Buffer (4));
      end Read_Chunk_ID;
   --
   --  Read 4 Bytes as Unsigned_32 (little endian) from File
      function Read_Uint32
        (BinF : in out Ada.Streams.Stream_IO.File_Type) return Unsigned_32 is
         Buffer : Ada.Streams.Stream_Element_Array (1 .. 4);
         Last : Ada.Streams.Stream_Element_Offset;
      begin
         Ada.Streams.Stream_IO.Read (BinF, Buffer, Last);
         return Unsigned_32 (Buffer (1)) +
                Unsigned_32 (Buffer (2)) * 2**8 +
                Unsigned_32 (Buffer (3)) * 2**16 +
                Unsigned_32 (Buffer (4)) * 2**24;
      end Read_Uint32;
   --
   --  Read 2 Bytes as Unsigned_16 (little endian)
      function Read_Uint16
        (BinF : in out Ada.Streams.Stream_IO.File_Type) return Unsigned_16 is
         Buffer : Ada.Streams.Stream_Element_Array (1 .. 2);
         Last : Ada.Streams.Stream_Element_Offset;
      begin
         Ada.Streams.Stream_IO.Read (BinF, Buffer, Last);
         return Unsigned_16 (Buffer (1)) +
                Unsigned_16 (Buffer (2)) * 2**8;
      end Read_Uint16;
   --
   --  Helper function to skip (jump over) Bytes
      procedure Skip_Bytes
        (BinF : in out Ada.Streams.Stream_IO.File_Type; Count : Positive) is
         Current_Index : Ada.Streams.Stream_IO.Positive_Count;
      begin
         Current_Index := Ada.Streams.Stream_IO.Index (BinF);
         Ada.Streams.Stream_IO.Set_Index
           (BinF,
            Current_Index + Ada.Streams.Stream_IO.Positive_Count (Count));
      end Skip_Bytes;
--

-----------------
   begin   --  Load_WAV_File
      --  Open file as binary stream
      Ada.Streams.Stream_IO.Open
        (BinF, Ada.Streams.Stream_IO.In_File, File_Name);
      --  Read RIFF header
      Header.ChunkID := Read_Chunk_ID (BinF); --  Read 4 char "RIFF"
      Header.ChunkSize := Read_Uint32 (BinF); --  Read 4 Bytes Unsigned_32
      Header.Format := Read_Chunk_ID (BinF);  --  "WAVE"

      --  Verify this is a WAV file
      if Header.ChunkID /= "RIFF" or else Header.Format /= "WAVE" then
         Put_Line ("Error: Not a valid WAV file");
         Ada.Streams.Stream_IO.Close (BinF);
         Result.Header := Header;
         if WAV_File.Data /= null then
            Free (WAV_File.Data);
            WAV_File.Data := null;
         end if;
         Data_B_Count   := 0;
         Data_W_Count_t := 1;
         Result.Data := null;
         return Result;
      end if;
--  RIFF and WAVE
--  Parse chunks until we find fmt and data
      Fmt_Found   := False;  --  Initialize
      Data_Found  := False;  --  Initialize
      while
         not (Fmt_Found and Data_Found) and then
         not Ada.Streams.Stream_IO.End_Of_File (BinF) loop
         --
         Chunk_ID := Read_Chunk_ID (BinF);  --  Chunk1ID "fmt "
         Chunk_Size := Read_Uint32 (BinF);  --  SubChunk1Size = 16

         if Chunk_ID = "fmt " then
         --  Read format chunk
            Header.Subchunk1ID := Chunk_ID;      --  "fmt "
            Header.Subchunk1Size := Chunk_Size;  --  16
            Header.AudioFormat := Read_Uint16 (BinF);
            Header.NumChannels := Read_Uint16 (BinF);
            Header.SampleRate := Read_Uint32 (BinF);
            Header.ByteRate := Read_Uint32 (BinF);
            Header.BlockAlign := Read_Uint16 (BinF);    --  Byte/Sample = 2
            Header.BitsPerSample := Read_Uint16 (BinF); --  16bit = 16
            --  Verify this is 16-bit audio
            if Header.BitsPerSample /= 16 then
               Put_Line ("Error: Only 16-bit audio is supported, found " &
                        Unsigned_16'Image (Header.BitsPerSample) & "-bit");
               Ada.Streams.Stream_IO.Close (BinF);
               Result.Header := Header;
               if WAV_File.Data /= null then
                  Free (WAV_File.Data);
                  WAV_File.Data := null;
               end if;
               Data_B_Count   := 0;
               Data_W_Count_t := 1;
--               Result.Data := null;
            end if;

         --  Skip any remaining bytes in the fmt chunk
            if Chunk_Size > 16 then
               Skip_Bytes (BinF, Natural (Chunk_Size - 16));
            end if;

            Fmt_Found := True;

         elsif Chunk_ID = "data" then
         --  Found data chunk
            Header.Subchunk2ID := Chunk_ID;       --  "data"
            Header.Subchunk2Size := Chunk_Size;   --  Number of PCM data Byte
            Data_B_Count := Natural (Chunk_Size);

            --  Calculate number of 16-bit samples
            Data_W_Count_t := Sample_Size_t (Chunk_Size / 2);

            --  allocated memory dynamically
            Data := new Sample_Array_16_t (1 .. Data_W_Count_t);

--  Read audio data
--  Treat Data array directly as Stream_Element_Array (overlay technique)
--  This overlays the same memory area with different types.
--  Safe because Byte and Stream_Element are the same size (8 bits).
            declare
               Data_Stream_View : Stream_Element_Array
                 (1 .. Stream_Element_Offset (Data_B_Count));
               for Data_Stream_View'Address use Data.all'Address;
               pragma Import (Ada, Data_Stream_View);
               Last : Stream_Element_Offset;
            begin
--  Note: WAV files use little-endian format
--  If your system is big-endian, you would need to swap bytes here
--  For most modern systems (x86, x64), no conversion is needed
               Stream_IO.Read (BinF, Data_Stream_View, Last);
            end;

            Data_Found := True;
         else
         --  Skip unknown chunks (like JUNK, LIST, etc.)
            Put_Line ("Skipping chunk: " & Chunk_ID & " (size:" &
                     Unsigned_32'Image (Chunk_Size) & ")");
            Skip_Bytes (BinF, Natural (Chunk_Size));

         --  Handle odd-sized chunks (WAV chunks must be word-aligned)
            if Chunk_Size mod 2 = 1 then
               Skip_Bytes (BinF, 1);
            end if;
         end if;
      end loop;  --  End of While loop

      --  Close file
      Ada.Streams.Stream_IO.Close (BinF);

      --  Check if we found both required chunks
      if not Fmt_Found then
         Put_Line ("Error: fmt chunk not found");

         if WAV_File.Data /= null then
            Free (WAV_File.Data);
            WAV_File.Data := null;
            Put_Line ("After Free (WAV_File.Data)");
         end if;
         Data_B_Count := 0;
      end if;

      if not Data_Found then
         Put_Line ("Error: data chunk not found");
         if Data /= null then
            Free (WAV_File.Data);
            WAV_File.Data := null;
         end if;
         Data_B_Count := 0;
      end if;

      --  Print header information
      Put_Line ("ChunkID=        " & Header.ChunkID);
      Put_Line ("ChunkSize=      " & Unsigned_32'Image (Header.ChunkSize));
      Put_Line ("Format=         " & Header.Format);
      Put_Line ("Subchunk1ID=    " & Header.Subchunk1ID);
      Put_Line ("Subchunk1Size=  " & Unsigned_32'Image (Header.Subchunk1Size));
      Put_Line ("Audio Format=   " & Unsigned_16'Image (Header.AudioFormat));
      Put_Line ("NumChannels=    " & Unsigned_16'Image (Header.NumChannels));
      Put_Line ("SampleRate=     " & Unsigned_32'Image (Header.SampleRate));
      Put_Line ("ByteRate=       " & Unsigned_32'Image (Header.ByteRate));
      Put_Line ("BlockAlign=     " & Unsigned_16'Image (Header.BlockAlign));
      Put_Line ("BitsPerSample=  " & Unsigned_16'Image (Header.BitsPerSample));
      Put_Line ("Subchunk2ID=    " & Header.Subchunk2ID);
      Put_Line ("Subchunk2Size=  " & Unsigned_32'Image (Header.Subchunk2Size));
      Put_Line ("PCM Word Count= " & Natural'Image (Data_B_Count / 2));

      --  Return result
      Result.Header := Header;
      Result.Data := Data;
      return Result;
   end Load_WAV_File;
--
--
--
----------------------------------------------------
-----------------------------------------------------
--   Sound Initialize
--   Open Wav file and setup all parameters
-----------------------------------------------------
-----------------------------------------------------
   procedure Audio_Initialize_Open is

   begin
   -------------------------
   --  Open WAV File
   -------------------------
--      if not Exists (To_String (UB_Sound_File_Name)) then
--         Put_Line ("The file name is not exist, cancelled");
--         return;
--      end if;
      if UB_Chooser_Filename_Only = UB_Sound_File_Name then
         Put_Line ("Skip file open and init, they were done already");
         return;    --  Already opened then do nothing
      end if;
      Put_Line ("Current File: " & To_String (UB_Sound_File_Name) &
                "   New File: " & To_String (UB_Chooser_Filename_Only));
--  New file open then clear current buffer
      Audio_End;

      UB_Sound_File_Name := UB_Chooser_Filename_Only;
--
      WAV_File := Load_WAV_File (To_String (UB_Sound_File_Name));
      if WAV_File.Data = null then
         Put_Line ("WAV File load error");
         return;
      end if;

      Put_Line ("Load WAV File Success");
--
--
   ---------------------------
   --  Open Default Playback Device to monitor
   --  "OpenAL_Soft" is Default. Pulse Audio to choose more
   ---------------------------
      Device := OpenAL.Context.Open_Default_Device;
      if Device = OpenAL.Context.Invalid_Device then
         Put_Line ("Error: Failed to open OpenAL device");
         return;
      end if;
--  Context works for 3D, Listner and Environment settings
      Context := Create_Context (Device);
      if Context = Null_Context then
         Put_Line ("Error: Failed to create OpenAL context");
         Close_Device (Device);
         return;
      end if;
--  Choose the working Context
      Set_Active_Context := Make_Context_Current (Context);
      if Set_Active_Context = False then
         Put_Line ("Error: Failed, cannot Set Active Context");
         Close_Device (Device);
         Destroy_Context (Context);
         return;
      end if;
      Put_Line ("Default Playback Device Opened");

----------------------------------------
--// Generate Buffers
--     C:  alGenBuffers(NUM_BUFFERS, g_Buffers);
--    Ada:  procedure Generate_Buffers (Buffers : in out Buffer_Array_t);
--    The Generate_Buffers procedure generates Buffers'Length buffers.
----------------------------------------
--      WAV_Byte_Length := Integer (WAV_File.Header.Subchunk2Size);
--      OpenAL.Thin.Gen_Buffers
--        (Size => OpenAL.Types.Size_t (WAV_Byte_Length / 2), -- Words
--         Buffers => Buffer_Arrt (1)'Address);              -- System.Address

--      Put_Line ("Generated Buffer, WAV_Length=" &
--               Integer'Image (WAV_Byte_Length) & " Byte");
      --

      OpenAL.Buffer.Generate_Buffers (WAV_Buffers);  --  1..S_COUNT_MAX Array
      OpenAL_Errort := OpenAL.Error.Get_Error;
      if OpenAL_Errort /= No_Error then
         Put_Line ("#####Buffer generation (WAV_Buffers) failed. Error: "
                   & OpenAL.Error.Error_t'Image (OpenAL_Errort));
      else
         Put_Line ("Generate_Buffers (WAV_Buffers) successful");
      end if;
------------
--

      for nnn in 1 .. S_COUNT_MAX loop

         if WAV_File.Data /= null then
            Put_Line ("WAV_File.Data exists, copying to AL_buffer");
            OpenAL.Buffer.Set_Data_Mono_16
                (Buffer    => WAV_Buffers (nnn),
                 Data      => WAV_File.Data.all,
                 Frequency => Frequency_t (WAV_File.Header.SampleRate));
--
            if OpenAL_Errort /= No_Error then
               Put_Line ("##### Set_Data_Mono_16 failed. Error: "
                         & OpenAL.Error.Error_t'Image (OpenAL_Errort));
               return;
            else
               Put_Line ("Set_Data_Mono_16 successful");
            end if;
         else
            Put_Line ("##### WAV_File is null. Copy was skipped #####");
         end if;
      end loop;

      --
--------------------------------------------------------------------
--  Generate Sources
--  alGenSources((ALuint)1, &source); //generates one or more sources,n=number
--  If error: alDeleteBuffers(NUM_BUFFERS, g_Buffers) ????
--  This was not written in the sample program
--------------------------------------------------------------------
      OpenAL.Source.Generate_Sources (Sound_Source_Array);  --  1..S_COUNT_MAX
--
      OpenAL_Errort := OpenAL.Error.Get_Error;
      Put_Line ("Generate_Sources Status = "
                & OpenAL.Error.Error_t'Image (OpenAL_Errort));
--
      if OpenAL.Source.Is_Valid (Sound_Source_Array (S_COUNT_MAX)) = True then
         Put_Line ("Source_Is_Valid=True GOOD");
      else
--            Close_Device (Device);
--            Destroy_Context (Context);
         Put_Line ("Source_Is_Valid=False BAD QUIT Initialize");
         Audio_End;
         return;
      end if;
--
---      end loop
--
--
-----------------------------------------------------------------------
--  // Attach buffer 0 to source
--  C: alSourcei(source[0], AL_BUFFER, g_Buffers[0]);
--  if ((error = alGetError()) != AL_NO_ERROR) {
--  DisplayALError("alSourcei AL_BUFFER 0 : ", error); }
--  procedure Set_Current_Buffer
--  (Source : in Source_t;
--  Buffer : in OpenAL.Buffer.Buffer_t);
----------------------------------------------------------------------
      for nnn in 1 .. S_COUNT_MAX loop
         OpenAL.Source.Set_Current_Buffer
           (Sound_Source_Array (nnn),
            WAV_Buffers (nnn));
      end loop;
--
--
--------------------------------------------------------------
--  Set_Looping
--  It can repeat the source sound
--  The second parameter is "Looping", should be True or False
--  It is True when repeating
--------------------------------------------------------------
      for nnn in 1 .. S_COUNT_MAX loop
         OpenAL.Source.Set_Looping
           (Source => Sound_Source_Array (nnn),
            Looping => True);
      end loop;
      Put_Line ("Source is set looping");
--
--
---------------------------
   --  Set OpenAL Doppler Effect Parameters
---------------------------
--  Convert to actual distance [meter]
      OpenAL.Global.Set_Speed_Of_Sound (343.3); --  Speed of sound in m/s

--  Doppler Factor
      OpenAL.Global.Set_Doppler_Factor (1.0);   --  Default Doppler factor

--  Rolloff Factor, emphasizes approaching and receding
      for nnn in 1 .. S_COUNT_MAX loop
         OpenAL.Source.Set_Rolloff_Factor_Float --  Default=1.0,Sensitive >1.0
           (Sound_Source_Array (nnn), Float_t (ROLLOFF_FACTOR));
      end loop;
--
--
--  Volume Control
      Audio_Set_Volumes;

      Audio_Dead_Zone := False;  --  Clear parameter
--
----------------------------------------------------------------------
      OpenAL.Listener.Set_Position_Float_List (Listener_Set_Position);

      OpenAL.Listener.Set_Velocity_Float_List (Listener_Set_Velocity);

      OpenAL.Listener.Set_Orientation_Float
        (Listener_Orientation_Forward, Listener_Orientation_Up);
--
   end Audio_Initialize_Open;
--
--
--
-------------------------------------------------------------
-------------------------------------------------------------
--  PLAY
-------------------------------------------------------------
-------------------------------------------------------------
--
--------------------------------------------------------------------
--  Play procedure sets the source specified by Source to the playing state.
--  with OpenAL.Source;
--  procedure Play (Source : in Source_t);
--------------------------------------------------------------------
   procedure Audio_Start is
   begin

--
      for nnn in 1 .. Natural (S_Count) loop
         OpenAL.Source.Play (Sound_Source_Array (nnn));
      end loop;
--      Put_Line ("Play(Sound_Source_Array (nnn))");
   end Audio_Start;
--
--
   procedure Audio_Stop is
   begin
      for nnn in 1 .. S_COUNT_MAX loop
         OpenAL.Source.Stop (Sound_Source_Array (nnn));
      end loop;
   end Audio_Stop;
   --

-------------------------------------
--  Set Audio Playing Parameters when S_Count=1 only
-------------------------------------
   procedure Audio_Playing_Parameters_1 is
      Previous_Dead_Zone : Boolean;
   begin
      Previous_Dead_Zone := Audio_Dead_Zone;  --  Store previou data
--  Set Current Position for Audio
      if Source_Direction = Right then
   --  Source Direction ==> Right
         if S_X_Pos_M < L_X_M then --  Now Left side then Source is Top
            SetPosF := OpenAL.Types.Float_t
               (S_X_Pos_M - L_X_M + S_X_Len_M / 2.0);
            if SetPosF > -0.01 then  --  Check Dead-zone
               SetPosF := -0.01;  --  In the Dead-zone then set Left limit
               Audio_Dead_Zone := True;
            else
               Audio_Dead_Zone := False;
            end if;
         else  --  Now Right side then Source is Tail
            SetPosF := OpenAL.Types.Float_t
               (S_X_Pos_M - L_X_M - S_X_Len_M / 2.0);
            if SetPosF < +0.01 then  --  Check Dead-zone
               SetPosF := +0.01;     --  In the Dead-zone then set Left limit
               Audio_Dead_Zone := True;
            else
               Audio_Dead_Zone := False;
            end if;
         end if;

      else
   --  Source_Direction <== Left
         if S_X_Pos_M < L_X_M then --  Now Left side then Source is Tail
            SetPosF := OpenAL.Types.Float_t
               (S_X_Pos_M - L_X_M + S_X_Len_M / 2.0);
            if SetPosF > -0.01 then  --  Passing Listner's Front
               SetPosF := -0.01;     --  -Dead
               Audio_Dead_Zone := True;
            else
               Audio_Dead_Zone := False;
            end if;
         else  --  Now Right side then Source is Top
            SetPosF := OpenAL.Types.Float_t
               (S_X_Pos_M - L_X_M - S_X_Len_M / 2.0);
            if SetPosF < +0.01 then  --  Passing Listner's Front
               SetPosF := +0.01;     --  +Dead
               Audio_Dead_Zone := True;
            else
               Audio_Dead_Zone := False;
            end if;
         end if;

      end if;
      OpenAL.Source.Set_Position_Float
        (Sound_Source_Array (1), SetPosF, 0.0, 0.0);

      OpenAL.Listener.Set_Position_Float (0.0, 0.0, Float_t (-L_Z_M));
--
--  Set Velocity
      if Audio_Dead_Zone then
      --  DEAD ZONE
      --  Just Front then Speed=0
         if not Previous_Dead_Zone then
         --  Moved into Dead zone
            OpenAL.Source.Set_Velocity_Float
              (Sound_Source_Array (1), 0.0, 0.0, 0.0);
         --  Adjust Gain UP 30% when Dead zone
         --  Check previous, change some setting if it's new
--
--
      -------------------------------------
      --  Gain Factor: The Deadzone Factor varies depending on Z (offset)
      --  Z = 0-0.9 m : x3
      --  Z = 1-1.9 m : x2
      --  Z = 2-9m    : x1.5
      --  Z = 10-50m  : x1.3
      --  Z >= 50m    : x1.2
      -------------------------------------
            declare
               Z : Float;
               Gain : Float;
            begin
               Z := abs (L_Z_M);
               if Z >= 0.0 and then Z <= 0.9 then
                  Gain := 3.0;
               elsif Z >= 1.0 and then Z <= 1.9 then
                  Gain := 2.0;
               elsif Z >= 2.0 and then Z <= 9.0 then
                  Gain := 1.5;
               elsif Z >= 10.0 and then Z <= 50.0 then
                  Gain := 1.3;
               else
                  Gain := 1.2;
               end if;
               OpenAL.Source.Set_Gain
                 (Sound_Source_Array (1),
                  Float_t (Float (S_Gain_Linear) * Gain));
               Put_Line
                 ("Deadzone Gain Factor=" &
                    Float'Image (Float (S_Gain_Linear) * Gain));
            end;
            OpenAL.Global.Set_Doppler_Factor (0.0);
         end if;  --  if not Previous Dead_Zone
            --
      else  --  Audio_Dead_Zone=False
      --  Not Deadzone, Regular position (Not just front)
         SetVelF := OpenAL.Types.Float_t (S_X_Speed * 1000.0 / 3600.0);
         if Source_Direction = Left then
            SetVelF := -SetVelF;
         end if;
         OpenAL.Source.Set_Velocity_Float
           (Sound_Source_Array (1), SetVelF, 0.0, 0.0);

         if Previous_Dead_Zone then
         --  Just after leaving the Dead Zone, return to the normal output
            OpenAL.Source.Set_Gain
              (Sound_Source_Array (1), Float_t (S_Gain_Linear));

            OpenAL.Source.Set_Rolloff_Factor_Float
              (Sound_Source_Array (1), 1.0);    --  Default=1.0,Sensitive >1.0
            OpenAL.Global.Set_Doppler_Factor (1.0); --  Default Doppler factor
         end if;
--
      end if;

   end Audio_Playing_Parameters_1;
--
--
-------------------------------------
--  Set Audio Playing Parameters when S_Count=2..10 (Not 1)
-------------------------------------
   procedure Audio_Playing_Parameters_m is
   begin
   --  Set Source Positions
      for nnn in 1 .. Natural (S_Count) loop
         SetPosF := OpenAL.Types.Float_t (S_X_Pos_M - L_X_M + S_X_Len_M / 2.0
                    - S_X_Len_M / (S_Count - 1.0) * Float (nnn - 1));

         OpenAL.Source.Set_Position_Float
           (Sound_Source_Array (nnn), SetPosF, 0.0, 0.0);
      end loop;

   --  Set Listener Position
      OpenAL.Listener.Set_Position_Float (0.0, 0.0, Float_t (-L_Z_M));

   --  Set Velocity
      SetVelF := OpenAL.Types.Float_t (S_X_Speed * 1000.0 / 3600.0);
      if Source_Direction = Left then
         SetVelF := -SetVelF;              --  Negate the sign
      end if;

      for nnn in 1 .. Natural (S_Count) loop
         OpenAL.Source.Set_Velocity_Float
           (Sound_Source_Array (nnn), SetVelF, 0.0, 0.0);
--         OpenAL.Source.Set_Gain
--           (Sound_Source_Array (nnn), Float_t (S_Gain_Linear));
      end loop;
   end Audio_Playing_Parameters_m;
--
--
-------------------------------------
--  Set Audio Volume
-------------------------------------
   procedure Audio_Set_Volumes is
   begin
      for nnn in 1 .. S_COUNT_MAX loop
         OpenAL.Source.Set_Gain
           (Sound_Source_Array (nnn), Float_t (S_Gain_Linear));
      end loop;
--
      OpenAL.Listener.Set_Gain (Float_t (L_Gain_Linear));
   end Audio_Set_Volumes;
--
--
----------------------------------------------------------
--  Audio END
----------------------------------------------------------
   procedure Audio_End is
      Success : Boolean;
      pragma Unreferenced (Success);
   begin
      Put_Line ("***Audio_Closing All*** begin");
      --  1. Stop Source
      for nnn in 1 .. S_COUNT_MAX loop
         OpenAL.Source.Stop (Sound_Source_Array (nnn));
      end loop;

      --  2. Unqueue Buffers
      for nnn in 1 .. S_COUNT_MAX loop
         OpenAL.Source.Get_Buffers_Queued (Source => Sound_Source_Array (nnn),
                                           Buffers => ProcessedNr);
         if ProcessedNr /= 0 then
            OpenAL.Source.Unqueue_Buffers (Source  => Sound_Source_Array (nnn),
                                     Buffers => WAV_Buffers);
         end if;
      end loop;

   --  3. Delete Source
      OpenAL.Source.Delete_Sources (Sound_Source_Array);

   --  4. Delete Buffers
      OpenAL.Buffer.Delete_Buffers (WAV_Buffers);

   --  5. Make context non-current
      Success := OpenAL.Context.Make_Context_Current
         (OpenAL.Context.Null_Context);

   --  6. Destroy Context
      OpenAL.Context.Destroy_Context (Context);

   --  7. Close Device
      OpenAL.Context.Close_Device (Device);

   --  8. Free PCM data
      if WAV_File.Data /= null then
         Put_Line ("Before Free (WAV_File.Data)");
         Put_Line ("Data_B_Count=" & Natural'Image (Data_B_Count));
         Free (WAV_File.Data);
         WAV_File.Data := null;
         Put_Line ("After Free (WAV_File.Data)");
      end if;
      Data_B_Count := 0;

      UB_Sound_File_Name := To_Unbounded_String ("");

      Put_Line ("***Audio_End*** processed");
   end Audio_End;

end Audio;
