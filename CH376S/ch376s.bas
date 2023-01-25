'-----------------------------------------------------------------------------------------

'name                     : CH376S

'copyright                : SAM

'purpose                  : test for CH376S

'micro                    : Mega16

'-----------------------------------------------------------------------------------------

$regfile = "m16def.dat"
$crystal = 4000000

$baud = 9600                                                ' use baud rate
$hwstack = 40                                               ' default use 32 for the hardware stack
$swstack = 40                                               ' default use 10 for the SW stack
$framesize = 400                                            ' default use 40 for the frame space

Config Serialout = Buffered , Size = 100
Config Serialin = Buffered , Size = 100

Open "comd.6:9600,8,n,1" For Output As #1
Open "comd.5:9600,8,n,1" For Input As #2

Dim Timeout As Integer
Dim Byte_tem As Byte
Dim Byte_in As Byte
Dim Str_tem As String * 50
Dim Wr_data As String * 50
Dim Wr_data_2 As String * 50

'Functions
Declare Function Serial_write(byval S As String)as Byte
Declare Function Serial_read(byval S As String)as Byte
Declare Function Print_command_header(header As String) As Byte
Declare Function Check_connection(byval Value As Byte)as Byte
Declare Function Set_usb_mode(byval Value As Byte)as Byte
Declare Function Reset_all()as Byte
Declare Function Read_file(byval File_name As String)as Byte
Declare Function Create_new_file(byval File_name As String)as Byte
Declare Function Create_and_write_file(byval File_name As String , Byval My_data As String)as Byte
Declare Function Append_file(byval File_name As String , Byval My_data As String)as Byte
Declare Function Set_file_name(byval File_name As String)as Byte
Declare Function Disk_connection_status()as Byte
Declare Function Usb_disk_mount()as Byte
Declare Function File_open()as Byte
Declare Function Set_byte_read(byval Num_bytes As Byte)as Byte
Declare Function Get_file_size()as Integer
Declare Function File_read()as Byte
Declare Function File_write(byval My_data As String)as Byte
Declare Function Continue_read()as Byte
Declare Function File_create()as Byte
Declare Function File_delete(byval File_name As String)as Byte
Declare Function File_pointer(byval File_beginning As Byte)as Byte
Declare Function File_close(byval Close_cmd As Byte)as Byte
Declare Function Wait_for_response(byval Errormsg As String)as Byte

Timeout = 2000
Wr_data = "I can do everything when i want."
Wr_data_2 = "Ich kann alles tun,wann ich will."

'It is important since UDRE interrupt is used that you enable the interrupts
Enable Interrupts

Str_tem = "Start"
Byte_tem = Serial_write(str_tem)


Do
   'get a char from the UART
   Byte_in = Inkey(#2)
   If Byte_in = 49 Then                                     'Input digit is 1
      Str_tem = "COMMAND1: CHECK CONNECTION"
      Byte_tem = Print_command_header(str_tem)
      Byte_tem = Check_connection(&H01)                     ' &H01

   Elseif Byte_in = 50 Then                                 'Input digit is 2
      Str_tem = "COMMAND2: set_USB_Mode"
      Byte_tem = Print_command_header(str_tem)
      Byte_tem = Set_usb_mode(&H06)                         ' &H06

   Elseif Byte_in = 51 Then                                 'Input digit is 3
      Str_tem = "COMMAND3: resetALL"
      Byte_tem = Print_command_header(str_tem)
      Byte_tem = Reset_all()

   Elseif Byte_in = 52 Then                                 'Input digit is 4
      Str_tem = "COMMAND4: Create and Write to file: FILE1.TXT"
      Byte_tem = Print_command_header(str_tem)
      Byte_tem = Create_and_write_file( "FILE1.TXT" , Wr_data)

   Elseif Byte_in = 53 Then                                 'Input digit is 5
      Str_tem = "COMMAND5: Read file: FILE1.TXT"
      Byte_tem = Print_command_header(str_tem)
      Byte_tem = Read_file( "FILE1.TXT")

   Elseif Byte_in = 54 Then                                 'Input digit is 6
      Str_tem = "COMMAND6: Append data to file: FILE1.TXT"
      Byte_tem = Print_command_header(str_tem)
      Byte_tem = Append_file( "FILE1.TXT" , Wr_data_2)

   Elseif Byte_in = 55 Then                                 'Input digit is 7
      Str_tem = "COMMAND7: Delete file: FILE1.TXT"
      Byte_tem = Print_command_header(str_tem)
      Byte_tem = File_delete( "FILE1.TXT")

   Elseif Byte_in = 56 Then                                 'Input digit is 8
      Str_tem = "COMMAND8: Read file: FILE2.TXT"
      Byte_tem = Print_command_header(str_tem)
      Byte_tem = Read_file( "FILE2.TXT")

   Elseif Byte_in = 57 Then                                 'Input digit is 9
      Str_tem = "COMMAND9: Create new file: FILE3.TXT"
      Byte_tem = Print_command_header(str_tem)
      Byte_tem = Create_new_file( "FILE3.TXT")

   End If

   If Ischarwaiting() = 1 Then
      Byte_tem = Inkey()
      Str_tem = Str(byte_tem)
      Byte_tem = Serial_write(str_tem)
   End If

Loop

Close #1
Close #2

'---------Serial_write-------------
Function Serial_write(byval S As String)

   Print #1 , S

End Function

'---------Serial_read--------------
Function Serial_read(byval S As String)

   Input #2 , S

End Function

'---------Print_command_header-----
Function Print_command_header(header As String)

   Byte_tem = Serial_write( "======================")
   Byte_tem = Serial_write(header)
   Byte_tem = Serial_write( "----------------------")

End Function

'---------Check_connection--------
'This function is used to check for successful communication with the CH376S module. This is not dependant of the presence of a USB stick.
'Send any value between 0 to 255, and the CH376S module will return a number = 255 - value.
Function Check_connection(byval Value As Byte)

   Local Value_inverse As Byte
   Local Byte_temp As Byte
   Local Str_temp As String * 40

   Value_inverse = 255 - Value

   Printbin &H57
   Printbin &HAB
   Printbin &H06
   Printbin Value

   Byte_temp = Wait_for_response( "checking connection")    'wait for a response from the CH376S. If CH376S responds, it will be true. If it times out, it will be false.

   If Byte_temp = Value_inverse Then
      Str_temp = ">Connection to CH376S was successful."
      Byte_tem = Serial_write(str_temp)
   Else
      Str_temp = ">Connection to CH376S - FAILED."
      Byte_tem = Serial_write(str_temp)
   End If

End Function

'---------Set_usb_mode------------
'Make sure that the USB is inserted when using 0x06 as the value in this specific code sequence
Function Set_usb_mode(byval Value As Byte)

   Local Usb_byte As Byte
   Local Byte_temp As Byte
   Local Str_temp As String * 40

   Printbin &H57
   Printbin &HAB
   Printbin &H15
   Printbin Value

   Waitms 10

   If Ischarwaiting() = 1 Then

      Usb_byte = Inkey()
      'Check to see if the command has been successfully transmitted and acknowledged.
      If Usb_byte = &H51 Then                               ' If true - the CH376S has acknowledged the command.

         Str_temp = "Set_usb_mode command acknowledged"
         Byte_temp = Serial_write(str_temp)
         Usb_byte = Inkey()
         'Check to see if the USB stick is connected or not.
         If Usb_byte = &H15 Then
            Byte_temp = Serial_write( "USB is present")     ' If the process was successful
         Else
            Str_temp = "USB Not present. Error code:"
            Byte_temp = Serial_write(str_temp)
            Str_temp = Str(usb_byte)                        ' If the USB is not connected - it should return an Error code = FFH
            Byte_temp = Serial_write(str_temp)
         End If

      Else
         Str_temp = "CH3765 error!   Error code:"
         Byte_temp = Serial_write(str_temp)
         Str_temp = Str(usb_byte)                           ' If the USB is not connected - it should return an Error code = FFH
         Byte_temp = Serial_write(str_temp)
      End If
   End If

   Waitms 1

End Function

'---------Reset_all------------
'This will perform a hardware reset of the CH376S module - which usually takes about 35 msecs =====
Function Reset_all()

   Local Byte_temp As Byte
   Local Str_temp As String * 40

   Printbin &H57
   Printbin &HAB
   Printbin &H05

   Waitms 50

   Str_temp = "The CH376S module has been reset!"
   Byte_temp = Serial_write(str_temp)

End Function

'---------Read_file------------
'This will send a series of commands to read data from a specific file (defined by fileName)
Function Read_file(byval File_name As String)

   Local Byte_temp As Byte
   Local File_size As Integer

   Byte_temp = Reset_all()                                  'Reset the module
   Byte_temp = Set_usb_mode(&H06)                           'Set to USB Mode
   Byte_temp = Disk_connection_status()                     'Check that communication with the USB device is possible
   Byte_temp = Usb_disk_mount()                             'Prepare the USB for reading/writing - you need to mount the USB disk for proper read/write operations.
   Byte_temp = Set_file_name(file_name)                     'Set File name
   Byte_temp = File_open()                                  'Open the file for reading
   File_size = Get_file_size()                              'Get the size of the file
   Byte_temp = File_read()                                  '***** Send the command to read the file ***
   Byte_temp = File_close(&H00)                             'Close the file

End Function

'---------Create_new_file------------
'is used to create a new file and then write data to that file. "fileName" is a variable used to hold the name of the file (e.g TEST.TXT). "data" should not be greater than 255 bytes long.
Function Create_new_file(byval File_name As String)

   Local Byte_temp As Byte
   Local Str_temp As String * 40

   Byte_temp = Reset_all()                                  'Reset the module
   Byte_temp = Set_usb_mode(&H06)                           'Set to USB Mode
   Byte_temp = Disk_connection_status()                     'Check that communication with the USB device is possible
   Byte_temp = Usb_disk_mount()                             'Prepare the USB for reading/writing - you need to mount the USB disk for proper read/write operations.
   Byte_temp = Set_file_name(file_name)                     'Set File name

   Byte_temp = File_create()
   If Byte_temp = 1 Then                                    'Try to create a new file. If file creation is successful
      Str_temp = "File successfully created"
      Byte_temp = Serial_write(str_temp)                       'write data to the file.
   Else
      Str_temp = "File could not be created, or it already exists"
      Byte_temp = Serial_write(str_temp)
   End If

   Byte_temp = File_close(&H01)

End Function

'---------Write_file------------
'is used to create a new file and then write data to that file. "fileName" is a variable used to hold the name of the file (e.g TEST.TXT). "data" should not be greater than 255 bytes long.
Function Create_and_write_file(byval File_name As String , Byval My_data As String)

   Local Byte_temp As Byte
   Local Str_temp As String * 40

   Byte_temp = Reset_all()                                  'Reset the module
   Byte_temp = Set_usb_mode(&H06)                           'Set to USB Mode
   Byte_temp = Disk_connection_status()                     'Check that communication with the USB device is possible
   Byte_temp = Usb_disk_mount()                             'Prepare the USB for reading/writing - you need to mount the USB disk for proper read/write operations.
   Byte_temp = Set_file_name(file_name)                     'Set File name

   Byte_temp = File_create()
   If Byte_temp = 1 Then                                    'Try to create a new file. If file creation is successful
      Byte_temp = File_write(my_data)                       'write data to the file.
   Else
      Str_temp = "File could not be created, or it already exists"
      Byte_temp = Serial_write(str_temp)
   End If

   Byte_temp = File_close(&H01)

End Function

'---------Append_file------------
'is used to write data to the end of the file, without erasing the contents of the file.
Function Append_file(byval File_name As String , Byval My_data As String)

   Local Byte_temp As Byte

   Byte_temp = Reset_all()                                  'Reset the module
   Byte_temp = Set_usb_mode(&H06)                           'Set to USB Mode
   Byte_temp = Disk_connection_status()                     'Check that communication with the USB device is possible
   Byte_temp = Usb_disk_mount()                             'Prepare the USB for reading/writing - you need to mount the USB disk for proper read/write operations.
   Byte_temp = Set_file_name(file_name)                     ' Set File Name
   Byte_temp = File_open()                                  'Open the file
   Byte_temp = File_pointer(0)                              'filePointer(false) is to set the pointer at the end of the file.  filePointer(true) will set the pointer to the beginning.
   Byte_temp = File_write(my_data)                          'Write data to the end of the file
   Byte_temp = File_close(&H01)                             'Close the file using 0x01 - which means to update the size of the file on close.

End Function

'---------Set_file_name------------
'This sets the name of the file to work with
Function Set_file_name(byval File_name As String)

   Local Byte_temp As Byte
   Local File_name_length As Byte

   File_name_length = Len(file_name)
   Byte_temp = Serial_write( "Setting file name to:")
   Byte_temp = Serial_write(file_name)

   Printbin &H57
   Printbin &HAB
   Printbin &H2F
   Printbin &H2F                                            ' ; File_name_length
   Print File_name ; Chr(&H00)

   Waitms 1

End Function

'---------Disk_connection_status------
'Check the disk connection status
Function Disk_connection_status()

   Local Byte_temp As Byte

   Byte_temp = Serial_write( "Checking USB disk connection status")

   Printbin &H57
   Printbin &HAB
   Printbin &H30

   Byte_temp = Wait_for_response( "Connecting to USB disk") 'wait for a response from the CH376S. If CH376S responds, it will be true. If it times out, it will be false.
   If Byte_temp = &H14 Then                                 'CH376S will send 0x14 if this command was successful
      Byte_temp = Serial_write( ">Connection to USB OK")
   Else
      Byte_temp = Serial_write( ">Connection to USB - FAILED.")
   End If

End Function

'---------Usb_disk_mount------------
'initialise the USB disk and check that it is ready - this process is required if you want to find the manufacturing information of the USB disk
Function Usb_disk_mount()

   Local Byte_temp As Byte

   Byte_temp = Serial_write( "Mounting USB disk")

   Printbin &H57
   Printbin &HAB
   Printbin &H31

   Byte_temp = Wait_for_response( "Mounting USB disk")      'wait for a response from the CH376S. If CH376S responds, it will be true. If it times out, it will be false.
   If Byte_temp = &H14 Then                                 'CH376S will send 0x14 if this command was successful
      Byte_temp = Serial_write( ">USB Mounted - OK")
   Else
      Byte_temp = Serial_write( ">Failed to Mount USB disk.")
   End If

End Function

'---------File_open------------
'opens the file for reading or writing
Function File_open()

   Local Byte_temp As Byte
   Local Str_temp As String * 40

   Byte_temp = Serial_write( "Opening file.")

   Printbin &H57
   Printbin &HAB
   Printbin &H32

   Byte_temp = Wait_for_response( "File open")              'wait for a response from the CH376S. If CH376S responds, it will be true. If it times out, it will be false.
   If Byte_temp = &H14 Then                                 'CH376S will send 0x14 if this command was successful
      Byte_temp = Serial_write( ">File opened successfully.")
   Else
      Byte_temp = Serial_write( ">Failed to open file.")
   End If

End Function

'---------Set_byte_read------------
'This function is required if you want to read data from the file.
Function Set_byte_read(byval Num_bytes As Byte)as Byte

   Local Byte_temp As Byte
   Local Bytes_to_read As Byte
   Local Time_counter As Integer
   Bytes_to_read = 0
   Time_counter = 0

   Printbin &H57
   Printbin &HAB
   Printbin &H3A
   Printbin Num_bytes
   Printbin &H00

   Byte_temp = Wait_for_response( "Set byte read")          ' Wait For A Response From The Ch376s. If Ch376s Responds , It Will Be True. If It Times Out , It Will Be False.
   If Byte_temp = &H1D Then Bytes_to_read = 1               'read the CH376S message. If equal to 0x1D, data is present, so return true. Will return 0x14 if no data is present.

   Set_byte_read = Bytes_to_read

End Function

'---------Get_file_size------------
'writes the file size to the serial Monitor.
Function Get_file_size()as Integer

   Local Byte_temp As Byte
   Local File_size As Integer
   Local Int_temp As Integer
   Local Str_temp As String * 40
   File_size = 0

   Byte_temp = Serial_write( "Getting File Size")
   Printbin &H57
   Printbin &HAB
   Printbin &H0C
   Printbin &H68

   Waitms 1

   Byte_temp = Serial_write( "FileSize =")
   If Ischarwaiting() = 1 Then
      File_size = File_size + Inkey()
   End If

   If Ischarwaiting() = 1 Then
      Int_temp = Inkey() * 255
      File_size = File_size + Int_temp
   End If

   If Ischarwaiting() = 1 Then
      Int_temp = Inkey() * 255
      Int_temp = Int_temp * 255
      File_size = File_size + Int_temp
   End If

   If Ischarwaiting() = 1 Then
      Int_temp = Inkey() * 255
      Int_temp = Int_temp * 255
      Int_temp = Int_temp * 255
      File_size = File_size + Int_temp
   End If

   Str_temp = Str(file_size)
   Byte_temp = Serial_write(str_temp)
   Waitms 1
   Get_file_size = File_size

End Function

'---------File_read------------
'read the contents of the file
Function File_read()

   Local Byte_temp As Byte
   Local Byte_temp_2 As Byte
   Local Str_temp As String * 40
   Local Str_temp_2 As String * 2
   Local First_byte As Byte
   Local Num_bytes As Byte
   Byte_temp_2 = 1
   First_byte = &H00                                        'Variable to hold the firstByte from every transmission.  Can be used as a checkSum if required.
   Num_bytes = &H40                                         'The maximum value is 0x40  =  64 bytes

   Do
      Byte_temp_2 = Set_byte_read(num_bytes)

      Printbin &H57
      Printbin &HAB
      Printbin &H27

      Waitms 1
      First_byte = Wait_for_response( "Reading data")       'Wait for the CH376S module to return data. TimeOut will return false. If data is being transmitted, it will return true.

      While Ischarwaiting() = 1
         Byte_temp = Inkey()
         Printbin #1 , Byte_temp
         Waitms 1
      Wend
      Byte_temp = Continue_read()
      If Byte_temp = 0 Then Exit Do                         'You need the continueRead() method if the data to be read from the USB device is greater than numBytes.

   'This tells the CH376S module how many bytes to read on the next reading step. In this example, we will read 0x10 bytes at a time. Returns true if there are bytes to read, false if there are no more bytes to read.
   Loop Until Byte_temp_2 = 0

   Byte_temp = Serial_write( " ")
   Byte_temp = Serial_write( "NO MORE DATA")

End Function

'---------File_write------------
'are the commands used to write to the file
Function File_write(byval My_data As String)

   Local Byte_temp As Byte
   Local Byte_temp_2 As Byte
   Local Str_temp As String * 40
   Local Data_length As Byte

   Data_length = Len(my_data)

   Byte_temp_2 = Serial_write( "Writing to file:")
   Byte_temp_2 = Serial_write(my_data)

   Str_temp = "Data Length:"
   Str_temp = Str_temp + Str(data_length)
   Byte_temp_2 = Serial_write(str_temp)
   Waitms 1

   'This set of commands tells the CH376S module how many bytes to expect from the Arduino.  (defined by the "dataLength" variable)
   Printbin &H57
   Printbin &HAB
   Printbin &H3C
   Printbin Data_length
   Printbin &H00

   Byte_temp = Wait_for_response( "Setting data Length")    ' Wait for an acknowledgement from the CH376S module before trying to send data to it
   If Byte_temp = &H1E Then                                 ' 0x1E indicates that the USB device is in write mode.

      'write the data to the file
      Printbin &H57
      Printbin &HAB
      Printbin &H2D
      Print My_data(1)

      Byte_temp = Wait_for_response( "Writing data to file")       ' wait for an acknowledgement from the CH376S module

      Byte_temp_2 = Serial_write( "Write code (normally FF and 14): ")

      'This is used to update the file size. Not sure if this is necessary for successful writing.
      Printbin &H57
      Printbin &HAB
      Printbin &H3D

      Byte_temp_2 = Wait_for_response( "Updating file size")       'wait for an acknowledgement from the CH376S module

      Str_temp = Hex(byte_temp)                             ' code is normally 0xFF
      Str_temp = Str_temp + " and "
      Str_temp = Str_temp + Hex(byte_temp_2)                'code is normally 0x14
      Byte_temp_2 = Serial_write(str_temp)
   End If

End Function

'---------Continue_read------------
'continue to read the file : I could not get this function to work as intended.
Function Continue_read()as Byte

   Local Byte_temp As Byte
   Local Read_again As Byte
   Read_again = 0

   Printbin &H57
   Printbin &HAB
   Printbin &H3B

   Byte_temp = Wait_for_response( "Continue read")          ' Wait For A Response From The Ch376s. If Ch376s Responds , It Will Be True. If It Times Out , It Will Be False.
   If Byte_temp = &H14 Then Read_again = 1                  'CH376S will send 0x14 if this command was successful

   Continue_read = Read_again

End Function

'---------File_create------------
'the command sequence to create a file
Function File_create()as Byte

   Local Byte_temp As Byte
   Local Created_file As Byte
   Created_file = 0

   Printbin &H57
   Printbin &HAB
   Printbin &H34

   Byte_temp = Wait_for_response( "Creating file")          'wait for a response from the CH376S. If file has been created successfully, it will return true.
   If Byte_temp = &H14 Then Created_file = 1                'CH376S will send 0x14 if this command was successful

   File_create = Created_file

End Function

'---------File_delete------------
'the command sequence to delete a file
Function File_delete(byval File_name As String)

   Local Byte_temp As Byte

   Byte_temp = Set_file_name(file_name)
   Waitms 1

   Printbin &H57
   Printbin &HAB
   Printbin &H35

   Byte_temp = Wait_for_response( "Deleting file")          'wait for a response from the CH376S. If file has been created successfully, it will return true.
   If Byte_temp = &H14 Then                                 'CH376S will send 0x14 if this command was successful
      Byte_temp = Serial_write( "Successfully deleted file")
   End If

End Function

'---------File_pointer------------
'is used to set the file pointer position. true for beginning of file, false for the end of the file.
Function File_pointer(byval File_beginning As Byte)

   Local Byte_temp As Byte

   Printbin &H57
   Printbin &HAB
   Printbin &H39

   If File_beginning = 1 Then
      'beginning of file
      Printbin &H00
      Printbin &H00
      Printbin &H00
      Printbin &H00
   Else
      'end of file
      Printbin &HFF
      Printbin &HFF
      Printbin &HFF
      Printbin &HFF
   End If

   Byte_temp = Wait_for_response( "Setting file pointer")   'wait for a response from the CH376S.
   If Byte_temp = &H14 Then                                 'CH376S will send 0x14 if this command was successful
      Byte_temp = Serial_write( "Pointer successfully applied")
   End If

End Function

'---------File_close------------
'closes the file
Function File_close(byval Close_cmd As Byte)

   Local Byte_temp As Byte
   Local Str_temp As String * 40
   Local Resp As Byte

   Byte_temp = Serial_write( "Closing file:")

   Printbin &H57
   Printbin &HAB
   Printbin &H36
   Printbin Close_cmd

   Resp = Wait_for_response( "Closing file")                'wait for a response from the CH376S.
   If Resp = &H14 Then                                      'CH376S will send 0x14 if this command was successful
      Byte_temp = Serial_write( ">File closed successfully.")
   Else
      Byte_temp = Serial_write( ">Failed to close file. Error code:")
      Str_temp = Str(resp)
      Byte_temp = Serial_write(str_temp)
   End If

End Function

'---------Wait_for_response---------
Function Wait_for_response(byval Errormsg As String)as Byte

   Local Response As Byte
   Local Count As Integer

   Response = 0
   Count = 0

   While Ischarwaiting() <> 1
      Waitms 1
      Count = Count + 1
      If Count > Timeout Then
         Byte_tem = Serial_write( "TimeOut. Error while: ")
         Byte_tem = Serial_write(errormsg)
         Exit While
      End If
   Wend

   If Ischarwaiting() = 1 Then
      Response = Inkey()
      Waitms 1
   End If

   Wait_for_response = Response

End Function