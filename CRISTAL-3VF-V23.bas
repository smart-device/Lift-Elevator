


' 1- prescaler dar config timer0 az 256 be 8 kahesh peida kard 88/5/27
' 2- moratabkardane barname
' 3- MOSHKELE DO DAR SUBROUTINE SET_CLOSE_RELAY
' OUT PUT RELAYS : NC   NO  RC   LIGHT  LOW   HIGH   DOWN   UP

   $regfile = "M128DEF.DAT"

   $hwstack = 1000
   $swstack = 1000

   Config Lcdpin = Pin , Db4 = Portd.5 , Db5 = Portd.4 , Db6 = Portd.1 , Db7 = Portd.0 , E = Portd.6 , Rs = Portd.7
   $crystal = 4000000

   Config Lcd = 16 * 2


   Config Timer0 = Timer , Prescale = 256
   'PRESCALE TIMER0 DAR BORDE VAGHEII BAYAD AFZAYESH YABAD.
   Enable Interrupts
   Enable Timer0
   Enable Ovf0
   On Ovf0 0s

      'BADAN MEGHDARE VAGHEII GHARAR GIRAD.
      'RELAY:dc-do-light-rc-low-high-dn-up
      'SEGMENT: ABCDEFG,UAL

      Config Portf = Input
      Config Portb = Output
      Config Porta = Input
      Config Portc = Input
      Config Portd = Output
      Config Portg = Output
      Config Porte = Output
      Config Portf = Input

      Config Pinc.4 = Output
      Config Pina.0 = Input
      Config Pina.1 = Input
      Config Pina.2 = Input

      Config Ping.0 = Input
      Config Ping.1 = Input
      Config Ping.2 = Input

      'Config Pinb.0 = Input
      'Config Pinb.1 = Input

      'Config Ping.3 = Output
      'Config Ping.4 = Output
      'Config Watchdog = 2048
      'Start Watchdog



      Config Timer1 = Timer , Prescale = 8
      'Timer1 Setting:
      Disable Interrupts
      Enable Timer1
      Enable Ovf1
      On Ovf1 1s
      Timer1 = 4700
      Gosub Timer_reset



      $baud = 11364
      Enable Interrupts
      Disable Urxc
      On Urxc Tt





      Dim Reg0 As Byte , Reg1 As Byte , Reg2 As Byte , I As Word , L1 As Byte , S1 As Byte , S2 As Byte
      Dim Mu_active As Bit , Md_active As Bit , Mu As Byte , Md As Byte , Stop_emr_buffer As Byte
      Dim Komak1 As Byte , Komak2 As Byte , Komak3 As Byte , Komak4 As Word , Komak5 As Word , Komak6 As Word
      Dim K5 As Byte
      Dim Level As Byte , Mu_fard As Byte , Md_fard As Byte , Jahat As Bit , Dorandaz As Bit
      Dim Sh_dakhel(32) As Byte , Sh_kharej(32) As Byte , J As Integer , Close_rev As Bit
      Dim Ww As Byte , Bb As Byte , Timer0_counter As Byte , Blink1 As Byte
      Dim Kkkk As Word , Nomerator(16) As Byte , Collect_byte As Byte
      Dim Qq As Byte , Pp As Byte , Kk As Byte , Kkk As Byte , Cc As Byte
      Dim Shasii_cancel_counter As Byte , Shasii_cancel_disable_bit As Bit
      Dim Timer_counter As Byte , Seconds As Byte , Timing_ok As Byte , Timer_buffer As Byte , Flag As Bit , Flag_time As Byte
      Dim Lock_error_bit As Bit , Seg_code As Byte , Code_k1 As Byte , Code_k2 As Byte
      Dim Mu_rom As Eram Byte , Md_rom As Eram Byte , Mojavez_stop As Byte

      Dim Page As Word , En_counter As Integer
      Dim M As Byte , Hh1 As Byte , Hh2 As Byte
      Dim U As Byte , U2 As Byte , M_u As Byte , M_d As Byte , Aa(100) As Byte , N As Byte
      Dim Second_door(16) As Eram Byte , Third_door(16) As Eram Byte
      Dim Call_clears_bit As Bit , Ee As Byte , Zz As Byte , Bt1 As Byte , Bt2 As Word
      Dim Door_mode_byte As Byte
      Dim Any_else_up_bit As Bit , Any_else_dn_bit As Bit , Y1 As Byte
      Dim Mood As Byte , First_setting1 As Eram Byte , Bb1 As Byte , L1_komaki As Byte
      Dim Close_err_flag As Byte , Dar_baz_ast As Bit
      Dim B1 As Byte , B2 As Byte , B3 As Byte , Mm As Byte , Nn As Byte
      Dim Meghdar1 As String * 3 , Row As Byte , Tedad As Bit , Dahgan As Byte , Address As Word , Sadgan As Byte
      Dim Komak7 As Byte , En_flag As Bit
      Dim Ser_con1 As Byte , Udr_k1 As Byte , Udr_k2 As Byte , Udr_k3 As Byte , Udr_k4 As Byte , Udr_k5 As Byte , Udr_k6 As Byte , Udr_k7 As Byte
      Dim Ser_con2 As Byte , Ser_level As Byte , Seg_ser As Byte
      Dim Rev_off As Byte , Rev_light_counter As Byte , Signal_word As Word , Signal_flag As Bit
      'Dim Do1_bit As Bit , Ol_bit As Bit , Dc_bit As Bit
      Dim Shasii_haman_tabaghe_bit As Byte , Read_shasii_emr As Bit , Write_shasii_emr As Bit
      Dim Close_err_counter As Bit
      Dim Haman_tabaghe_counter As Byte
      Dim Count As Integer
      Dim Door_contact_counter As Byte
      Dim Door_contact_counter_photocell As Byte
      Dim Level_buffer As Byte
      Dim Counting As Integer
      Dim Move_counter As Integer
      Dim Park_floor As Byte
      Dim Tes As Byte
      Dim Rev_up_first_time As Bit
      Rev_up_first_time = 0

      Dim Rev_dn_first_time As Bit
      Rev_dn_first_time = 0

      Gosub 7seg


      Door_contact_counter_photocell = 0
      Move_counter = 1
      Door_contact_counter = 0


      Declare Sub Test(byval Loghat As String , Byval X1 As Byte , Byval Y1 As Byte)

      Fb Alias Pinc.1
      'Fire_lifter Alias Reg0.1
      Program Alias Ping.0
      Up Alias Ping.1
      Dn Alias Ping.2

      'B1:
      Rd Alias Pinc.5
      Mu_in Alias Pinc.3
      Md_in Alias Pinc.2

      'B2:
      Final Alias Pina.0
      D_con Alias Pina.1
      L_con Alias Pina.3
      S_stop Alias Pina.2
      Dls Alias Pina.4
      Ds Alias Pina.5
      Uls Alias Pina.6
      Us Alias Pina.7
      485_en Alias Portc.4
      Rev Alias Pinc.7
      Ru Alias Pinc.6
      Ptc Alias Pinc.0

      L1_l Alias Porte.7
      S2_l Alias Portg.3
      S1_l Alias Portg.4
      Direct Alias Porte.2
      Hall_k1 Alias Porte.6
      Hall_k2 Alias Porte.3

      Do1_bit Alias Udr_k1.0
      Dc_bit Alias Udr_k1.1
      Ol_bit Alias Udr_k1.2


      L1 = 255
      S1 = 0
      S2 = 0

      Haman_tabaghe_counter = 0                             ' counter shassi haman tabaghe sefr ast

      Portb = L1
      Set L1_l
      Reset L1_l
      Portb = S1
      Set S1_l
      Reset S1_l
      Portb = S2
      Set S2_l
      Reset S2_l
      L1 = 0
      Gosub 0s




      Call_clears_bit = 1
      Mu = 1
      Md = 1
      Level = 1
      Jahat = 1
      Flag = 0

      For I = 1 To 32
            Sh_dakhel(i) = 1
            Sh_kharej(i) = 1
      Next I


      L1 = &B00100000
      Cls
      Cursor Off
      Waitms 200


      Call Test( "*  www.i-s.ir  *" , 1 , 1)
      Call Test( "*  66747792    *" , 2 , 1)

      Call_clears_bit = 1
      Waitms 400
      Call_clears_bit = 1
      Waitms 400
      Call_clears_bit = 1
      Waitms 400
      Call_clears_bit = 1
      Waitms 400
      Call_clears_bit = 1
      Waitms 400
      Call_clears_bit = 1
      Waitms 400

      Cls
      Locate 1 , 1
      Lcd "Tel#0919-4986646"
      Wait 1
      Gosub New_data

      Call_clears_bit = 1

      Cls
      Locate 1 , 1
      Lcd " *** Crystal ***"
      Locate 2 , 1
      Lcd "UP TO "
      Meghdar1 = Lookupstr(aa(76) , M_data)
      Lcd Meghdar1
      Lcd " LEVELS."
      Call_clears_bit = 1
      Wait 2
      Call_clears_bit = 1
      Gosub Reading_data
      'LOAD VORODIHA
      Gosub 0s
      Gosub Reading_data

      Start Timer0


      Cursor Off
      Cls
      Mu = Mu_rom
      Md = Md_rom
      If Mu = 0 Or Mu = 255 Then Mu = 1
      If Md = 0 Or Md = 255 Then Md = 1

      Komak1 = First_setting1
      If Komak1 = 255 Then Gosub Setting
      Gosub Magnet_display

      Call Test( "Dat94- Ver23-3VF" , 1 , 1)
      Call_clears_bit = 1
      Waitms 400
      Call_clears_bit = 1
      Waitms 400
      Call_clears_bit = 1
      Waitms 400


      Enable Urxc

      Call_clears_bit = 1
      Waitms 400
      Call_clears_bit = 1
      Waitms 400

'////////////////////////// MAIN ///////////////////////////////////////////////
Main:



            Mood = 4
            L1 = &B00100000
            Hwstack = 605
            Flag = 0

            If Signal_flag = 0 Then Call Test( "READY           " , 1 , 1)
            If Signal_flag = 1 Then Call Test( "READY,NO SIGNAL!" , 1 , 1)

            Gosub Timer_reset
            Gosub Magnet_display


            'LIGHT TIME
            If Aa(61) < 8 Then
               Komak6 = Aa(61) * 10
               Komak5 = Komak6 + 20
            End If
            If Aa(61) > 7 Then
               Komak6 = Aa(61) - 3
               Komak5 = Komak6 * 20
            End If

            Gosub Timer_reset
            Timer_buffer = Komak5
            Start Timer1


            If Door_mode_byte > 9 And Aa(77) = 1 Then Goto Door_mode_error

            Do
                  If Signal_flag = 0 Then Call Test( "READY           " , 1 , 1)
                  If Signal_flag = 1 Then Call Test( "READY,NO SIGNAL!" , 1 , 1)
                  Gosub 7seg
                  Gosub Shenasaii2
                  Gosub Bache_halghe
                  Waitms 100
                  If Timing_ok = 1 Then
                        'PARK FLOOR:
                        Zz = Aa(75)
                        Zz = Zz - 1
                        If Level <> Zz And Zz <> 0 Then
                              Gosub Timer_reset
                              Sh_kharej(zz) = 0
                              Gosub Shasii_cancel
                              Shasii_cancel_disable_bit = 1
                              Waitms 200
                              Gosub Bache_halghe
                        End If
                        Gosub Timer_reset
                        Exit Do
                  End If
            Loop
                                                                                      'parking mode darbe close ya open bashad
            If Aa(74) = 2 Then L1 = &B10000000 Else L1 = &B00000000

            If Aa(73) = 1 Then
                  Blink1 = 7
                  Do
                        If Signal_flag = 0 Then Call Test( "READY           " , 1 , 1)
                        If Signal_flag = 1 Then Call Test( "READY,NO SIGNAL!" , 1 , 1)
                        K5 = 0
                        Set K5.blink1
                        S1 = K5
                        S2 = K5
                        Gosub Bache_halghe
                        Waitms 50
                        'Gosub Bache_halghe
                        Blink1 = Blink1 - 1
                        If Blink1 = 1 Then
                              Blink1 = 7
                              S1 = 0
                              Waitms 500
                              Gosub 7seg
                              Waitms 400
                        End If
                  Loop
            End If


            If Aa(73) = 2 Then
                  Do
                        If Signal_flag = 0 Then Call Test( "READY           " , 1 , 1)
                        If Signal_flag = 1 Then Call Test( "READY,NO SIGNAL!" , 1 , 1)
                        Waitms 300
                        Gosub Bache_halghe
                        Gosub Timer_reset
                  Loop
            End If

Goto Main


'///////////////////////////////////////////////////////////////////////////////

Check_ds:
         If L_con = 1 Then Return
         If Ds = 0 Then Return
         Waitms 50
         If L_con = 1 Then Return
         'AGAR REV BOD,SEG,MAGNET NAMAYESH DAHAT,RELAY=0,BARGARDAD
         If Ds = 1 Then
               Gosub Check_stop_emr
               'INJA BAYAD DARB DASTI BAZ SHAVAD TA BA FARMANE RD JAHAT NAGIRAD.
               Mu = 1
               Md = 1
               Level = 1
               Gosub 7seg
               L1 = &B00100000
               Gosub Write_mu_md
               Gosub Timer_reset
               Gosub Magnet_display
               Gosub Set_open_relay

               'PAIINTARIN TABAGHE NESHAN DAHAD VA BE  MAIN BERAVAD
               'KARE SHENASAII TAMAM SHODE VA BE MAIN MIRAVAD.
               If Rev = 0 Then Goto Main
         End If
Return

'///////////////////////////////////////////////////////////////////////////////

Check_dls:


'      If Jahat = 0 And Uls = 1 Then
'            Gosub Call_clears
'            Gosub Timer_reset
'            L1 = &B10111010
            ' Gosub Stop_move_uls_dls
            'Gosub Timer_reset
'Haminja_dls_bemon:
'            If Md = 1 And Md = 1 Then L1 = &B10100000
'            Call Test( "  ULS DLS Error " , 1 , 1)
'            Goto Haminja_dls_bemon
'      End If



      If L_con = 1 Then Return
      If Dls = 0 Then Return
      Waitms 50
      If L_con = 1 Then Return
         Gosub Check_rev
         If Dls = 1 Then
            L1 = &B10111010
            Dorandaz = 1
            Mu = 2
            Md = 2
            Level = 1
            Call Test( "SLOW DOWN        " , 1 , 1)
            Gosub Magnet_display
      Do
            Gosub Check_rev
            Gosub Check_stop_emr
            Level = 1
            Gosub 7seg

            If Mu_in = 1 And Md_in = 1 And Rev = 0 Then
               Mu = 1
               Md = 1
               Level = 1
               Gosub Magnet_display
               Gosub Timer_reset
               Gosub Stop_move
               Gosub Timer_reset
               If Rev = 0 Then Goto Main
            End If
      Loop

      End If

Return


'///////////////////////////////////////////////////////////////////////////////

Check_us:


      If Us = 1 Then Return
      Waitms 50
      If Us = 1 Then Return
      If Us = 0 Then
            Waitms 100
            Gosub Check_stop_emr
            L1 = &B00100000
            Gosub Write_mu_md
            Gosub Timer_reset
            Gosub 7seg
            Gosub Set_open_relay
            ''Balatarin Tabaghe Neshan Dahad Va Be Main Beravad
            ''Kare Shenasaii Tamam Shode Va Be Main Miravad.
            If Rev = 0 Then Goto Main
      End If

Return

'///////////////////////////////////////////////////////////////////////////////


Check_uls:


'      If Jahat = 1 And Dls = 1 Then
'            Gosub Call_clears
'            Gosub Timer_reset
'            L1 = &B10111001
            'Gosub Stop_move_uls_dls
            'Gosub Timer_reset
'Haminja_uls_bemon:
'            If Md = 1 And Md = 1 Then L1 = &B10100000
'            Call Test( "  ULS DLS Error " , 1 , 1)
'            Goto Haminja_uls_bemon
'      End If

      If L_con = 1 Then Return
      If Uls = 0 Then Return
      Waitms 50
      If L_con = 1 Then Return
      L1 = &B10111001                                       'RELAY LOW,UP=1
      Gosub Check_rev
      Dorandaz = 1
      Komak4 = Aa(76) * 2                                   'MOHASEBE DORANDAZE EJBARIYE BALA BAR ASASE TEDADE TABAGHAT
      Mu = Komak4 - 2
      Md = Komak4 - 2
      Gosub Magnet_display
      Gosub 7seg
      Call Test( "SLOW UP         " , 1 , 1)

   Do
      Gosub Check_rev
      Gosub Check_stop_emr
      Level = Level_buffer
      Gosub 7seg

      If Mu_in = 1 And Md_in = 1 And Rev = 0 Then
            Komak4 = Aa(76) * 2
            Mu = Komak4 - 1
            Md = Komak4 - 1
            Gosub Stop_move                                 ' JAHAT VA HIGH VA LOW BARDASHTE MISHAVD O BARAYE BAZ KARDANE DARB MIRAVAD
            Gosub Timer_reset
            If Rev = 0 Then Goto Main
      End If
    Loop

Return

'///////////////////////////////////////////////////////////////////////////////

Check_feedback:
      If Fb = 1 Then

            If Rev = 1 Then
                  If Fb = 0 Then Return
                  Waitms 100
                  If Fb = 0 Then Return
                  Waitms 100
                  If Fb = 0 Then Return
           End If


            Close_err_flag = 1
            Call_clears_bit = 1
            Call Test( "CONTACTOR FAULT     " , 1 , 1)
            Do
                  L1 = &B01100000
                  Seg_code = 3
                  Gosub Code
                  If Fb = 0 And Rev = 0 Then Goto Pain_haminja_fb
                  If Rev = 1 Then Goto Pain_haminja_fb
            Loop
      End If
Pain_haminja_fb:
Return


'///////////////////////////////////////////////////////////////////////////////


Check_door_open_photocell:
         If Rev = 1 Then Return
         If Do1_bit = 1 Then
            If Rev = 1 Then Exit Do
            Close_err_flag = 1
            Call Test( "PHOTOCELL DO    " , 1 , 1)
            Dar_baz_ast = 0

            Gosub Timer_reset
            Komak4 = Aa(63) + 2
            Timer_buffer = Komak4
            Start Timer1


            If Rev = 0 Then Gosub Set_open_relay_bi_lcd

            Do
                  If Timing_ok = 1 Then
                     Gosub Timer_reset
                     Exit Do
                  End If

                  If Do1_bit = 0 Then Exit Do

                  Door_contact_counter_photocell = Door_contact_counter_photocell + 1
                  If Door_contact_counter_photocell > 8 Then
                        Call_clears_bit = 1
                        Waitms 400
                        Gosub Timer_reset
                        Gosub Call_clears
                        Gosub Shasii_cancel
                        Gosub Shasii_cancel_disable
                        Door_contact_counter_photocell = 0
                        Goto Main
                  End If


                  Seg_code = 5
                  Gosub Check_rev
                  Shasii_cancel_disable_bit = 1
                  Gosub Code
                  If Rev = 1 Then Return
            Loop

          End If
:

Return

'///////////////////////////////////////////////////////////////////////////////

Check_overheat:

         If Ptc = 0 Then
            Call Test( "OVERHEAT PTC    " , 1 , 1)
            If Rev = 0 Then Gosub Set_open_relay_bi_lcd

            Do
                  If Ptc = 1 Then Goto Main
                  Call_clears_bit = 1
                  Waitms 400
                  Gosub Call_clears
                  Gosub Timer_reset
                  Gosub Shasii_cancel
                  Gosub Shasii_cancel_disable

                  L1 = &B01100000
                  Close_err_flag = 1
                  L1 = &B01100000
                  Shasii_cancel_disable_bit = 1
                  Seg_code = 9
                  Gosub Code
                  If Rev = 1 Then Return
            Loop
         End If


Return

'///////////////////////////////////////////////////////////////////////////////

Check_doorcont:

         If D_con = 1 Then

               If D_con = 0 Then Return
               Close_err_flag = 1
               Call Test( "DOOR IS OPEN    " , 1 , 1)
               Dar_baz_ast = 0
               If Rev = 0 Then Gosub Set_open_relay_bi_lcd

               Do
                     If D_con = 0 Then Exit Do

                        'Door_contact_counter = Door_contact_counter + 1
                        'If Door_contact_counter > 10 Then
                        '      Call_clears_bit = 1
                        '      Waitms 400
                        '      Gosub Timer_reset
                        '      Gosub Call_clears
                        '      Gosub Shasii_cancel
                        '      Gosub Shasii_cancel_disable
                        '      Door_contact_counter = 0
                        '      Goto Main
                        'End If


                     Seg_code = 5
                     Gosub Code
                     If Rev = 1 Then Return
                     Shasii_cancel_disable_bit = 1
               Loop

         End If
Return

'///////////////////////////////////////////////////////////////////////////////

Check_overload:

      If Rev = 1 Then Return
      If Ol_bit = 1 Then
            'TAMAME RELAYHA KHAMOOSH
            L1 = &B01100000
            Close_err_flag = 1
            Call Test( "OVERLOAD        " , 1 , 1)
            Gosub Set_open_relay_bi_lcd
            Call_clears_bit = 1
            Waitms 400
            Gosub Call_clears
            Gosub Shasii_cancel
            Gosub Shasii_cancel_disable


            Do
                  If Ol_bit = 0 Then Goto Main
                  Call_clears_bit = 1
                  Waitms 400
                  Gosub Call_clears
                  Gosub Shasii_cancel
                  Gosub Shasii_cancel_disable
                  Shasii_cancel_disable_bit = 1
                  Seg_code = 1
                  Gosub Code
                  If Rev = 1 Then Return
            Loop

      End If
Return

'///////////////////////////////////////////////////////////////////////////////

Set_close_relay:

            If L_con = 0 Then Return

            If Do1_bit = 1 Then                             'BIT BAZ BODANE DARB
                  If Rev = 1 Then Goto Biroone_set_close_if
                  L1 = &B01100000
                  Dar_baz_ast = 0
                  Gosub Set_open_relay
                  Seg_code = 5
                  Gosub Code
                  Goto Main
             End If

Biroone_set_close_if:

            Gosub Timer_reset
            Komak4 = Aa(63) + 3
            Timer_buffer = Komak4
            Start Timer1

            If Do1_bit = 1 Then
               If Rev = 1 Then Goto Biroone_do1_bit
               L1 = &B01100000
               Goto Main
            End If
Biroone_do1_bit:
            Komak4 = Aa(72)

            If Komak4 <> 2 Then Wait 2

            If D_con = 1 Then
                     Dar_baz_ast = 0
                     Gosub Set_open_relay
                     Seg_code = 5
                     Gosub Code
                     L1 = &B01100000
                     Goto Main
            End If

            Call Test( "CLOSE DOOR       " , 1 , 1)

            L1 = &B10100000
            Do

               If S_stop = 0 Then
                   L1 = &B10110000
               End If


                     Set L1.7
                     Gosub Check_doorcont

                     If Rev = 0 Then
                           Gosub Check_door_open_photocell
                           Gosub Shasii_haman_tabaghe
                     End If

                     Shasii_cancel_disable_bit = 1
                     If L_con = 0 Then
                           Wait 1
                           Exit Do
                     End If


                     'AUTO:
                     If Rev = 0 Then
                           If Do1_bit = 1 Or D_con = 1 Then
                                 Dar_baz_ast = 0
                                 Gosub Set_open_relay
                                 Seg_code = 5
                                 Gosub Code
                                 Goto Main
                           End If
                     End If

                     'REV:
                     If Rev = 1 Then
                           If D_con = 1 Then
                                 Dar_baz_ast = 0
                                 Gosub Set_open_relay
                                 Seg_code = 5
                                 Gosub Code
                                 Goto Main
                           End If
                     End If

                     If Rev = 1 And Ru = 1 And Rd = 1 Then
                           Gosub Set_open_relay
                           Return
                     End If


                     If Timing_ok = 1 Then
                           Gosub Timer_reset
                           Exit Do
                     End If
            Loop



            If S_stop = 1 And Close_err_counter = 1 Then Goto S_stop_error
            If L_con = 1 And Close_err_counter = 1 Then Goto Lock_error

            Lock_error_bit = 0
            Dar_baz_ast = 0

            If S_stop = 1 Or L_con = 1 Then
                  Close_err_counter = 1
                  Gosub Set_open_relay
                  Goto Main
            End If


Return


'///////////////////////////////////////////////////////////////////////////////
Opening_error:
         'AGAR DARB BAZ SHOD BE MAIN BERAVAD
         L1 = &B01100000
         Call Test( "OPEN FAULT        " , 1 , 1)
         Call_clears_bit = 1
         Seg_code = 4
         Gosub Code
         Gosub Timer_reset
         Gosub Check_rev
         If L_con = 1 Then
            Return
         End If

Goto Opening_error




'///////////////////////////////////////////////////////////////////////////////

Sub Test(byval Loghat As String , X1 As Byte , Byval Y1 As Byte)
      Disable Interrupts
      Locate X1 , Y1
      Lcd Loghat
      Enable Interrupts
End Sub

'///////////////////////////////////////////////////////////////////////////////


0s:
      Reset Watchdog
      Stop Watchdog


      Signal_word = Signal_word + 1
      If Signal_word > 50 Then Signal_flag = 1

      For Hh1 = 0 To 7
         Hh2 = 7 - Hh1
         If L1.hh1 = 0 Then Set L1_komaki.hh2 Else Reset L1_komaki.hh2
      Next Hh1


      Ser_con1 = Ser_con1 + 1

      If Ser_con1 > 2 Then
            Ser_con1 = 0
            Ser_con2 = Ser_con2 + 1

            If Ptc = 0 Then
               Udr = 70
               Waitus 800
            End If

            If Ser_con2 > 14 Then Ser_con2 = 1

            Set 485_en

                  If Ser_con2 = 1 Then
                     Udr = 51
                     Waitus 800
                  End If

                  If Ser_con2 = 2 Then
                     Udr = 52
                     Waitus 800
                  End If

                  If Ser_con2 = 3 Then
                     Udr = 53
                     Waitus 800
                  End If

                  If Ser_con2 = 4 Then
                     Udr = 54
                     Waitus 800
                  End If

                  'SHASII_CANCEL:
                  If Ser_con2 = 5 And L1_komaki.7 = 1 And L1_komaki.6 = 1 Then
                     Ser_level = Level + 100
                     Udr = Ser_level
                     Waitus 800
                  End If

                  '7SEG:
                  If Ser_con2 = 6 Then
                     Udr = Seg_ser
                     Waitus 800
                  End If

            'LIGHT:
                  If Ser_con2 = 7 Then
                     If L1_komaki.2 = 0 Then
                        Udr = 55
                        Waitus 800
                     End If
                     If L1_komaki.2 = 1 Then
                        Udr = 56
                        Waitus 800
                     End If
                  End If

            'DC:
                  If Ser_con2 = 8 Then
                     If L1_komaki.0 = 0 Then
                        Udr = 57
                        Waitus 800
                     End If
                     If L1_komaki.0 = 1 Then
                        Udr = 58
                        Waitus 800
                     End If
                  End If


                  If Ser_con2 = 9 Then
                        'UAL
                        If L1_komaki.7 = 0 Then
                           Udr = 59
                           Waitus 800
                        End If

                        'DAL
                        If L1_komaki.6 = 0 Then
                           Udr = 60
                           Waitus 800
                        End If

                        'UAL,DAL=OFF
                        If L1_komaki.7 = 1 And L1_komaki.6 = 1 Then
                           Udr = 61
                           Waitus 800
                        End If

                  End If

                  If Ser_con2 = 10 And Call_clears_bit = 1 Then
                        Udr = 62
                        Waitus 800
                        Gosub Call_clears
                        Call_clears_bit = 0
                  End If

                  If Ser_con2 = 11 And Read_shasii_emr = 1 Then
                        Udr = 63
                        Waitus 800
                  End If

                  If Ser_con2 = 12 And Write_shasii_emr = 1 Then
                        Udr = 64
                        Waitus 800
                  End If

                  'HIGH:
                  If Ser_con2 = 13 Then
                        If L1_komaki.5 = 0 Then
                              Udr = 65
                              Waitus 800
                              End If
                              If L1_komaki.5 = 1 Then
                              Udr = 66
                              Waitus 800
                        End If
                  End If

                  'D_CON:
                  If Ser_con2 = 14 Then
                        If D_con = 0 Then
                              Udr = 67
                              Waitus 800
                              End If
                              If D_con = 1 Then
                              Udr = 68
                              Waitus 800
                        End If
                  End If
            Reset 485_en

      End If

      If L1.0 = 1 Then Set S1.0 Else Reset S1.0             'UAL
      If L1.1 = 1 Then Set S2.0 Else Reset S2.0             'DAL

      Portb = L1_komaki
      Set L1_l
      Reset L1_l
      Portb = S1
      Set S1_l
      Reset S1_l
      Portb = S2
      Set S2_l
      Reset S2_l


      Config Portf = Input

      'HALL KEY
      Set Direct
      Reset Hall_k1
      For Bb = 0 To 7
         Cc = Bb + 17
         Ww = Bb + 1
         If Pinf.bb = 0 Then Sh_kharej(ww) = 0 Else Sh_kharej(ww) = 1
         If Aa(cc) = 2 Then Sh_kharej(ww) = 1
      Next Bb
      Set Hall_k1


      Set Direct
      Reset Hall_k2
      For Bb = 0 To 7
         Cc = Bb + 25
         Ww = Bb + 9
         If Pinf.bb = 0 Then Sh_kharej(ww) = 0 Else Sh_kharej(ww) = 1
         If Aa(cc) = 2 Then Sh_kharej(ww) = 1
      Next Bb
      Set Hall_k2

      For Bb = 0 To 7
         Ww = Bb + 1
         Cc = Bb + 9
         If Udr_k3.bb = 0 Then Sh_dakhel(ww) = 0 Else Sh_dakhel(ww) = 1
         If Udr_k2.bb = 0 Then Sh_dakhel(cc) = 0 Else Sh_dakhel(cc) = 1
      Next Bb

      Config Portf = Output

      If Shasii_cancel_disable_bit = 1 Then
         Gosub Shasii_cancel_disable
         Shasii_cancel_disable_bit = 0
      End If

      If Program = 0 Then En_counter = En_counter + 1
      If Program = 1 Then En_counter = 0
      If En_counter = 100 Then En_flag = 1

      If D_con = 1 Then Door_mode_byte = 0

Return


'//////////////////////////////////////////////////////////////////////////////

Close_door:

      Close_err_flag = 0
      If L1.0 = 0 And L1.1 = 0 Then
            Gosub Check_doorcont
            Gosub Check_overheat
      End If

      ''AGAR REV NABASHAD:
      If Rev = 0 Then
            If L1.0 = 0 And L1.1 = 0 Then
                  Gosub Check_overload
                  Gosub Check_door_open_photocell
            End If
      End If

      If Rev = 1 And Ru = 1 And Rd = 1 Then Goto Check_rev
      If Close_err_flag = 1 Then Goto Close_door

      If L_con = 0 And D_con = 0 Then
            If L1.0 = 0 And L1.1 = 0 Then Gosub Check_feedback
            Lock_error_bit = 0
            Dar_baz_ast = 0
            Return
      End If

      Gosub Set_close_relay
      If L1.0 = 0 And L1.1 = 0 Then Gosub Check_feedback

      Waitms 200
Return

'//////////////////////////////////////////////////////////////////////////////


Check_rev:
      'FLAG TIME:

      If Rev = 1 Then
            Waitms 1
            If Rev = 0 Then Return
            Gosub Call_clears
            Call_clears_bit = 1
            Gosub Timer_reset
            Flag = 0
            Mu_active = 1
            Md_active = 1


            Goto Check_revmove
      End If

Return

'//////////////////////////////////////////////////////////////////////////////

Check_revmove:
      Mood = 1



         Reset L1.0
         Reset L1.1
         Reset L1.2
         Reset L1.3
         Reset L1.4
         Reset L1.6
         Set L1.7

         Rev_up_first_time = 1
         Rev_dn_first_time = 1


Check_revup:
      If Rev = 0 Then Goto Revesion_out
      Gosub Call_clears
      If Rd = 0 And Ru = 1 Then Goto Check_revdn
      If Ru = 0 And Rd = 1 Then

            Rev_off = 0
            Rev_light_counter = 0
            Gosub 7seg
            Gosub Timer_reset
            Jahat = 1
            Call_clears_bit = 1
            Gosub Close_door
            Close_rev = 1

            If Uls = 1 Then
                  Call Test( "REVISION UP  ULS " , 1 , 1)
                  L1 = &B10100000
                  Komak4 = Aa(76) * 2
                  Mu = Komak4 - 2
                  Md = Komak4 - 2
                  Gosub Write_mu_md
                  Gosub Timer_reset
                  Gosub Magnet_display
                  Gosub 7seg
                  Goto Check_revmove
            End If

            'AGAR DA BASTANE DARB,FARMAN BARDASHTE SHODE BASHAD.
            If Ru = 1 Then Goto Check_revmove
            'If Us = 0 Then
            Call Test( "REVISION UP     " , 1 , 1)
            'If Us = 1 Then Call Test( "REVISION UP US  " , 1 , 1)

            If Mu_in = 1 And Mu_active = 0 Then
                  Mu = Mu + 1
                  Mu_active = 1
            End If
            If Md_in = 1 And Md_active = 0 Then
                  Md = Md + 1
                  Md_active = 1
            End If
            If Mu_in = 0 Then Mu_active = 0
            If Md_in = 0 Then Md_active = 0
            If Md = 0 Then Md = 1
            If Mu = 0 Then Mu = 1
            Gosub Write_mu_md

            Gosub Magnet_display

            If Rev_up_first_time = 1 Then
                        L1 = &B10110001
                        Rev_up_first_time = 0
                        Waitms 100
            End If

            L1 = &B10111001

            Stop_emr_buffer = &B10111001                    'RELAY LIGHT'UP,LOW=1
            Gosub Check_stop_emr
            Goto Check_revup

      End If

'//////////////////////////////////////////////////////////////////////////////


Check_revdn:
      If Rev = 0 Then Goto Revesion_out
      Gosub Call_clears
      If Ru = 0 And Rd = 1 Then Goto Check_revup
      If Rd = 0 And Ru = 1 Then

            Rev_off = 0
            Rev_light_counter = 0
            Gosub 7seg
            Gosub Timer_reset
            Jahat = 0
            Call_clears_bit = 1
            Gosub Close_door
            Close_rev = 1


            If Dls = 1 Then
                  Call Test( "REVISION DN  DLS " , 1 , 1)
                  L1 = &B10100000
                  Dorandaz = 1
                  Mu = 2
                  Md = 2
                  Level = 1
                  Gosub Magnet_display
                  Gosub Check_stop_emr

                  Gosub Write_mu_md
                  Gosub Timer_reset
                  Gosub Magnet_display

                  Goto Check_revmove
            End If


            'AGAR DA BASTANE DARB,FARMAN BARDASHTE SHODE BASHAD.
            If Rd = 1 Then Goto Check_revmove
            'If Ds = 0 Then
            Call Test( "REVISION DOWN   " , 1 , 1)
            'If Ds = 1 Then Call Test( "REVISION DN DS  " , 1 , 1)

            If Mu_in = 1 And Mu_active = 0 Then
                  Mu = Mu - 1
                  Mu_active = 1
            End If
            If Md_in = 1 And Md_active = 0 Then
                  Md = Md - 1
                  Md_active = 1
            End If
            If Mu_in = 0 Then Mu_active = 0
            If Md_in = 0 Then Md_active = 0
            Gosub Write_mu_md

            Gosub Magnet_display


            If Rev_dn_first_time = 1 Then
                        L1 = &B10110010
                        Rev_dn_first_time = 0
                        Waitms 100
            End If
            L1 = &B10111010



            Stop_emr_buffer = &B10111010                    'RELAY LIGHT'DOWN,LOW=1
            Gosub Check_stop_emr

            Goto Check_revdn
      End If



      If Rev = 0 Then Goto Revesion_out
      If Signal_flag = 0 Then Call Test( "REVISION        " , 1 , 1)
      If Signal_flag = 1 Then Call Test( "REV  ,NO SIGNAL!" , 1 , 1)

      Seg_ser = 39
      S1 = &B00001010
      S2 = 0
      Call_clears_bit = 1


      Timer_buffer = 100
      Start Timer1

      If Timing_ok = 1 And Rev_off = 0 Then
         Rev_light_counter = Rev_light_counter + 1

         If Rev_light_counter = 4 Then
               Rev_off = 1
               Rev_light_counter = 0
         End If
         Gosub Timer_reset
      End If


      If Rev_off = 1 Then Reset L1.5 Else Set L1.5
      Waitms 10
      Gosub Mu_md_fard_zoj

      If Mu_fard = 1 And Md_fard = 1 And Mu = Md Then

         'Set L1.6
         'Reset L1.0
         'Reset L1.1
         'Reset L1.2
         'Reset L1.3
         'Reset L1.4
         'Wait 2
         'Reset L1.7

      'Else

         Set L1.7
         Reset L1.6
         Reset L1.0
         Reset L1.1
         Reset L1.2
         Reset L1.3
         Reset L1.4

      End If

      If Rev_off = 1 Then Reset L1.5 Else Set L1.5




      Gosub Check_programing
      Gosub Ck_final
      Goto Check_revmove

'///////////////////////////////////////////////////////////////////////////////

Magnet_display:
         Gosub Mu_md_fard_zoj
         If Jahat = 1 Then
               If Aa(71) = 2 Then
                     Komak4 = Mu / 2
                     Level = Komak4 + 1
               End If
               If Aa(71) = 1 Then Level = Mu
               If Level = 0 Then Level = 1
               Level_buffer = Level
         End If

         If Jahat = 0 Then
               If Aa(71) = 2 Then
                     Komak5 = Md
                     If Md_fard = 1 Then Komak5 = Md + 1
                     Level = Komak5 / 2
               End If
               If Aa(71) = 1 Then Level = Md
               If Level = 0 Then Level = 1
               Level_buffer = Level
         End If


         Disable Interrupts
         Locate 2 , 1
         Lcd "FL:"
         Lcd Level
         If Level < 10 Then Lcd " "

         If Mu = 0 Then Mu = 1
         If Md = 0 Then Md = 1
         Locate 2 , 6
         Lcd "SU:"
         Lcd Mu
         If Mu < 10 Then Lcd " "

         Locate 2 , 12
         Lcd "SD:"
         Lcd Md
         If Md < 10 Then Lcd " "
         Enable Interrupts
Return

'//////////////////////////////////////////////////////////////////////////////


Mu_md_fard_zoj:
      Md_fard = 0
      Komak1 = Md / 2
      Komak2 = Komak1 * 2
      If Komak2 <> Md Then Md_fard = 1

      Mu_fard = 0
      Komak1 = Mu / 2
      Komak2 = Komak1 * 2
      If Komak2 <> Mu Then Mu_fard = 1
Return

'//////////////////////////////////////////////////////////////////////////////


Shasii_up:
      J = Level + 1
      For I = J To Aa(76)
         If Sh_dakhel(i) = 0 Or Sh_kharej(i) = 0 Then
               Jahat = 1
               Goto Move_up
         End If
      Next I
Return

'//////////////////////////////////////////////////////////////////////////////

Shasii_dn:
      J = Level - 1
      For I = 1 To J
      If Sh_dakhel(i) = 0 Or Sh_kharej(i) = 0 Then
      Jahat = 0
      Goto Move_dn
      End If
      Next I
Return

'//////////////////////////////////////////////////////////////////////////////


Move_up:
      Wait 1
      Mood = 2
      Door_mode_byte = Door_mode_byte + 1
      Gosub Timer_reset
      Gosub Check_rev
      Jahat = 1
      Gosub 7seg
      If D_con = 1 Then Wait 1
      If D_con = 1 Then Wait 1
      If D_con = 1 Then Wait 1
      Gosub Close_door
      Waitms 100
      Gosub Check_uls

      L1 = &B10110001
      Waitms 200
      L1 = &B10110101

      Stop_emr_buffer = &B10110101
      Waitms 10

      Call Test( "HIGH UP            " , 1 , 1)
      Shasii_haman_tabaghe_bit = 0
      Mu_active = 1
      Md_active = 1
      Dorandaz = 0
      Gosub Timer_reset
      Timer_buffer = Flag_time
      Start Timer1
      Flag = 1
      Waitms 300






Do
      If Ptc = 0 Then
              Call_clears_bit = 0
              Qq = 0
              Pp = 0
              Call Test( "PTC Move Up     " , 1 , 1)
                    For Bb = 0 To 7
                        Ww = Bb + 1
                        Cc = Bb + 9
                        Udr_k3.bb = 0
                        Sh_dakhel(ww) = 0
                        Udr_k2.bb = 0
                        Sh_dakhel(cc) = 0
                    Next Bb


      End If


      Gosub Check_rev
      Gosub Check_mu
      Gosub Check_md
      Gosub 7seg
      Gosub Ck_sare_tabaghe
      Shasii_cancel_disable_bit = 1
      Gosub Check_uls
      Gosub Check_stop_emr
Loop


'//////////////////////////////////////////////////////////////////////////////


Move_dn:
      Wait 1
      Mood = 3
      Door_mode_byte = Door_mode_byte + 1
      Gosub Timer_reset
      Gosub Check_rev
      Jahat = 0
      Gosub 7seg
      If D_con = 1 Then Wait 1
      If D_con = 1 Then Wait 1
      If D_con = 1 Then Wait 1
      Gosub Close_door
      Waitms 100
      Gosub Check_dls


      L1 = &B10110010
      Waitms 200
      L1 = &B10110110

      Stop_emr_buffer = &B10110110
      Waitms 10

      Call Test( "HIGH DOWN         " , 1 , 1)
      Shasii_haman_tabaghe_bit = 0
      Mu_active = 1
      Md_active = 1
      Dorandaz = 0
      Gosub Timer_reset
      Timer_buffer = Flag_time
      Start Timer1
      Flag = 1
      Waitms 300


Do
      If Ptc = 0 Then
              Call_clears_bit = 0
              Qq = 0
              Pp = 0
              Call Test( "PTC Move Dn     " , 1 , 1)
                    For Bb = 0 To 7
                        Ww = Bb + 1
                        Cc = Bb + 9
                        Udr_k3.bb = 0
                        Sh_dakhel(ww) = 0
                        Udr_k2.bb = 0
                        Sh_dakhel(cc) = 0
                    Next Bb


      End If


      Gosub Check_rev
      Gosub Check_mu
      Gosub Check_md
      Gosub 7seg
      Gosub Ck_sare_tabaghe
      Shasii_cancel_disable_bit = 1
      Gosub Check_dls
      Gosub Check_stop_emr
Loop


'//////////////////////////////////////////////////////////////////////////////


Check_mu:
      If Mu_in = 0 Then Mu_active = 0
      If Mu_active = 1 Then Return
      If Mu_in = 0 Then Return
      Waitms 20
      If Mu_in = 0 Then Return

      'Gosub Ck_sare_tabaghe
      If Jahat = 1 And Dorandaz = 1 Then Return
      If Jahat = 1 Then Mu = Mu + 1
      If Jahat = 0 Then Mu = Mu - 1
      Gosub Timer_reset
      Timer_buffer = Flag_time
      Start Timer1
      Flag = 1
      Gosub Magnet_display
      If Mu_fard = 1 Then Goto Biroone_check_mu
      If Jahat = 1 Then Gosub If_slow
Biroone_check_mu:
      Mu_active = 1
Return

'//////////////////////////////////////////////////////////////////////////////


Check_md:
      If Md_in = 0 Then Md_active = 0
      If Md_active = 1 Then Return
      If Md_in = 0 Then Return
      Waitms 20
      If Md_in = 0 Then Return

      'Gosub Ck_sare_tabaghe
      If Jahat = 0 And Dorandaz = 1 Then Return
      If Jahat = 1 Then Md = Md + 1
      If Jahat = 0 Then Md = Md - 1
      Gosub Timer_reset
      Timer_buffer = Flag_time
      Start Timer1
      Flag = 1
      Gosub Magnet_display
      If Md_fard = 1 Then Goto Biroone_check_md
      If Jahat = 0 Then Gosub If_slow
Biroone_check_md:
      Md_active = 1
Return


'//////////////////////////////////////////////////////////////////////////////

If_slow:
      For I = 1 To 16
         If Sh_dakhel(i) = 0 Then
               If Level = I Then Gosub If_slow_sub
         End If
         If Sh_kharej(i) = 0 Then
            Gosub Any_else
            If Level = I Then
               Collect_byte = 83 + Level
               'TABAGHE BASE:
               'If Jahat = 1 And Aa(4) = Level Then Gosub If_slow_sub
               If Jahat = 1 And Any_else_up_bit = 1 And Aa(collect_byte) = 3 Then Goto Here3
               If Jahat = 0 And Any_else_dn_bit = 1 And Aa(collect_byte) = 2 Then Goto Here3
               If Aa(71) <> 1 Then Gosub If_slow_sub Else Goto Stop_move
            End If
         End If
Here3:
      Next I
Return

'//////////////////////////////////////////////////////////////////////////////

If_slow_sub:
      Dorandaz = 1
      If Jahat = 1 Then
            Call Test( "SLOW UP          " , 1 , 1)

            L1 = &B10111001
            Stop_emr_buffer = &B10111001
            Waitms 300
      End If
      If Jahat = 0 Then
            Call Test( "SLOW DOWN        " , 1 , 1)
            L1 = &B10111010

            Stop_emr_buffer = &B10111010
            Waitms 300
      End If
Return

'//////////////////////////////////////////////////////////////////////////////

Any_else:
      Any_else_up_bit = 0
      Any_else_dn_bit = 0
      For Y1 = 1 To 32
            If Sh_dakhel(y1) = 0 Or Sh_kharej(y1) = 0 And Level < Y1 Then Any_else_up_bit = 1
            If Sh_dakhel(y1) = 0 Or Sh_kharej(y1) = 0 And Level > Y1 Then Any_else_dn_bit = 1
      Next Y1

Return

'//////////////////////////////////////////////////////////////////////////////

Ck_sare_tabaghe:
      If Dorandaz = 1 Then
         If Mu_in = 0 Or Md_in = 0 Then Mojavez_stop = 1
         If Mojavez_stop = 0 Then Return
      End If
      If Dorandaz = 1 And Mu_in = 1 And Md_in = 1 Then
         Dorandaz = 0
         Komak4 = Level * 2
         Komak4 = Komak4 - 1
         Mu = Komak4
         Md = Komak4
         Goto Stop_move
      End If
Return

'//////////////////////////////////////////////////////////////////////////////

Stop_move:


      Flag = 0
      Mojavez_stop = 0
      Gosub Write_mu_md
      Gosub Timer_reset
      Gosub 7seg
      If Jahat = 1 Then Md = Mu
      If Jahat = 0 Then Mu = Md
      Gosub Magnet_display
      '3VF:
      'AVAL JAHAT BARDASHTE MISHAVAD

      'UP STOP TIME:
      If Aa(66) <> 1 And Jahat = 1 Then
            Bt1 = Aa(66)
            Bt2 = Bt1
            Bt2 = Bt2 * 500
            L1 = &B10110001
            Stop_emr_buffer = &B10110001
            Waitms Bt2
      End If
      'DN STOP TIME:
      If Aa(67) <> 1 And Jahat = 0 Then
            Bt1 = Aa(67)
            Bt2 = Bt1
            Bt2 = Bt2 * 500
            L1 = &B10110010
            Stop_emr_buffer = &B10110010
            Waitms Bt2
      End If


      L1 = &B10100000                                       ' JAHAT VA HIGH VA LOW BARDASHTE MISHAVD O BARAYE BAZ KARDANE DARB MIRAVAD
      Call Test( "OPEN DOOR        " , 1 , 1)
      Dar_baz_ast = 0


      If Aa(66) <> 1 And Jahat = 1 Then
            Bt1 = Aa(66)
            Bt2 = Bt1
            Bt2 = Bt2 * 400
            Waitms Bt2
      End If
      'DN STOP TIME:
      If Aa(67) <> 1 And Jahat = 0 Then
            Bt1 = Aa(67)
            Bt2 = Bt1
            Bt2 = Bt2 * 400
            Waitms Bt2
      End If

      Wait 1.5


      Gosub Set_open_relay


Goto Main


'///////////////////////////////////////////////////////////////////////////////

Stop_move_uls_dls:


      Flag = 0
      Mojavez_stop = 0
      Gosub Write_mu_md
      Gosub Timer_reset
      Gosub 7seg
      If Jahat = 1 Then Md = Mu
      If Jahat = 0 Then Mu = Md
      Gosub Magnet_display
      '3VF:
      'AVAL JAHAT BARDASHTE MISHAVAD

      'UP STOP TIME:
      If Aa(66) <> 1 And Jahat = 1 Then
            Bt1 = Aa(66)
            Bt2 = Bt1
            Bt2 = Bt2 * 100
            L1 = &B10110001
            Stop_emr_buffer = &B10110001
            Waitms Bt2
      End If
      'DN STOP TIME:
      If Aa(67) <> 1 And Jahat = 0 Then
            Bt1 = Aa(67)
            Bt2 = Bt1
            Bt2 = Bt2 * 100
            L1 = &B10110010
            Stop_emr_buffer = &B10110010
            Waitms Bt2
      End If


      L1 = &B10100000                                       ' JAHAT VA HIGH VA LOW BARDASHTE MISHAVD O BARAYE BAZ KARDANE DARB MIRAVAD
      Dar_baz_ast = 0


      If Aa(66) <> 1 And Jahat = 1 Then
            Bt1 = Aa(66)
            Bt2 = Bt1
            Bt2 = Bt2 * 100
            Waitms Bt2
      End If
      'DN STOP TIME:
      If Aa(67) <> 1 And Jahat = 0 Then
            Bt1 = Aa(67)
            Bt2 = Bt1
            Bt2 = Bt2 * 100
            Waitms Bt2
      End If
      Gosub Set_open_relay
Return


'///////////////////////////////////////////////////////////////////////////////

Numerator_data:
   Data " " , "-1" , "-2" , "-3" , "-4" , "P" , "b" , "G" , "0" , "1" , "2" , "3" , "4" , "5" , "6" , "7" , "8" , "9" , "10" , "11" , "12" , "13" , "14" , "15" , "16" , "17" , "18" , "19" , "20"

'///////////////////////////////////////////////////////////////////////////////

7seg_data_s2:
   Data 0 , 2 , 2 , 2 , 2 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 96 , 96 , 96 , 96 , 96 , 96 , 96 , 96 , 96 , 96 , 218 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0

'///////////////////////////////////////////////////////////////////////////////

7seg_data_s1:
   Data 0 , 96 , 218 , 242 , 102 , 206 , 62 , 188 , 252 , 96 , 218 , 242 , 102 , 182 , 190 , 224 , 254 , 246 , 252 , 96 , 218 , 242 , 102 , 182 , 190 , 224 , 254 , 246 , 252 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0

'///////////////////////////////////////////////////////////////////////////////
7seg:
      Komak4 = Level + 34
      Komak1 = Aa(komak4)
      Seg_ser = Komak1
      S1 = Lookup(komak1 , 7seg_data_s1)
      S2 = Lookup(komak1 , 7seg_data_s2)
Return

'///////////////////////////////////////////////////////////////////////////////

Check_stop_emr:
         If D_con = 0 And L_con = 0 Then Return
         For B1 = 1 To 50
            If D_con = 0 And L_con = 0 Then Return
            Waitms 1
         Next B1


         For Kkk = 1 To 8
            Kk = Kkk - 1
            If Sh_kharej(kkk) = 0 Then Reset Nn.kk Else Set Nn.kk
         Next Kkk

         For Kkk = 9 To 16
             Kk = Kkk - 9
             If Sh_kharej(kkk) = 0 Then Reset Mm.kk Else Set Mm.kk
         Next Kkk



         L1 = &B10100000
         Gosub 7seg
         Call Test( "STOP EMERJENSY    " , 1 , 1)
         Gosub Timer_reset


         Read_shasii_emr = 1
         Waitms 900
         Read_shasii_emr = 0
         Call_clears_bit = 1
         Waitms 500


         Do

            L1 = &B10100000
            Gosub Check_rev
            Call_clears_bit = 1
            If D_con = 0 Then
                  Gosub Check_rev
                  Gosub Set_close_relay
                  Call_clears_bit = 1


                  Call_clears_bit = 0
                  Waitms 100

                  Waitms 1
                  Waitms 1


                  Stop Timer0

                  For Kkk = 1 To 8
                  Kk = Kkk - 1
                  If Mm.kk = 0 Then Reset Sh_dakhel(kkk) Else Set Sh_dakhel(kkk)
                  Next Kkk
                  For Kkk = 9 To 16
                  Kk = Kkk - 9
                  If Nn.kk = 0 Then Reset Sh_dakhel(kkk) Else Set Sh_dakhel(kkk)
                  Next Kkk



                  Reset Hall_k1
                  Config Portf = Output
                  Reset Direct
                  Portf = Nn
                  'Waitms 1
                  Set Direct
                  Set Hall_k1


                  Reset Hall_k2
                  Config Portf = Output
                  Reset Direct
                  Portf = Mm
                  'Waitms 1
                  Set Direct
                  Set Hall_k2


                  Waitms 100
                  Start Timer0
                  Waitms 100


                  Write_shasii_emr = 1
                  Call_clears_bit = 0
                  Wait 1
                  Write_shasii_emr = 0


                  L1 = Stop_emr_buffer
                  If Stop_emr_buffer.0 = 1 And Stop_emr_buffer.2 = 1 Then Call Test( "HIGH UP           " , 1 , 1)
                  If Stop_emr_buffer.0 = 1 And Stop_emr_buffer.3 = 1 Then Call Test( "SLOW UP           " , 1 , 1)
                  If Stop_emr_buffer.1 = 1 And Stop_emr_buffer.2 = 1 Then Call Test( "HIGH DOWN         " , 1 , 1)
                  If Stop_emr_buffer.1 = 1 And Stop_emr_buffer.3 = 1 Then Call Test( "SLOW DOWN         " , 1 , 1)
                  'IF <> REV THEN START FLAG TIME AGAIN:

                  If Rev = 0 Then
                        Gosub Timer_reset
                        Timer_buffer = Flag_time
                        Start Timer1
                        ''SET TIMER FOR FLAG TIME
                        Flag = 1
                  End If
                  Return

            End If
         Loop

'\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\

Shasii_cancel:


      'SHASII BALATAR EZAFE RA CANCEL KONAD:
      For Kkk = 1 To 32
      If Kkk > Aa(76) Then Sh_kharej(kkk) = 1
      If Kkk > Aa(76) Then Sh_dakhel(kkk) = 1
      Next Kkk



      For Kkk = 1 To 8
         Kk = Kkk - 1
         If Sh_kharej(kkk) = 0 Then Reset Pp.kk Else Set Pp.kk
      Next Kkk

      For Kkk = 9 To 16
         Kk = Kkk - 9
         If Sh_kharej(kkk) = 0 Then Reset Qq.kk Else Set Qq.kk
      Next Kkk





      If Level < 9 Then
            Kkk = Level - 1
            Set Pp.kkk
      End If

      If Level > 8 Then
            Kkk = Level - 9
            Set Qq.kkk
      End If

      If Level < 9 Then Kkk = Level - 1
      If Level > 8 Then Kk = Level - 9

      Reset Hall_k1
      Config Portf = Output
      Reset Direct
      Portf = Pp
      Set Direct
      Set Hall_k1
      'End If

      'If Level > 8 Then Kkk = Level - 9

      Reset Hall_k2
      Config Portf = Output
      Reset Direct
      Portf = Qq
      Set Direct
      Set Hall_k2
'End If

Return


'\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\

Shasii_cancel_disable:

      'SHASII BALATAR EZAFE RA CANCEL KONAD:
      For Kkk = 1 To 32
      If Kkk > Aa(76) Then Sh_kharej(kkk) = 1
      If Kkk > Aa(76) Then Sh_dakhel(kkk) = 1
      Next Kkk

      For Kkk = 1 To 8
      Kk = Kkk - 1
      If Sh_kharej(kkk) = 0 Then Reset Pp.kk Else Set Pp.kk
      Next Kkk
      For Kkk = 9 To 16
      Kk = Kkk - 9
      If Sh_kharej(kkk) = 0 Then Reset Qq.kk Else Set Qq.kk
      Next Kkk


      Reset Hall_k1
      Config Portf = Output
      Reset Direct
      Portf = Pp
      Set Direct
      Set Hall_k1


      Reset Hall_k2
      Config Portf = Output
      Reset Direct
      Portf = Qq
      Set Direct
      Set Hall_k2


Return


'\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\

Call_clears:
      Qq = 255
      Pp = 255


      Reset Hall_k1
      Config Portf = Output
      Reset Direct
      Portf = Qq
      'Waitms 1
      Set Direct
      Set Hall_k1


      Reset Hall_k2
      Config Portf = Output
      Reset Direct
      Portf = Pp
      'Waitms 1
      Set Direct
      Set Hall_k2


      For Ee = 1 To 16
      Sh_dakhel(ee) = 1
      Sh_kharej(ee) = 1
      Next Ee

      Waitms 1
      Config Portf = Input

Return



'\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\

1s:
      Timer_counter = Timer_counter + 1
      If Timer_counter = 8 Then
            Timer_counter = 0
            Seconds = Seconds + 1

            If Flag_time = Seconds And Flag = 1 Then Goto Flag_error
            If Timer_buffer = Seconds Or Timer_buffer < Seconds Then
                  Seconds = 0
                  Timer_buffer = 0
                  Timing_ok = 1
                  Goto Here20
            End If
      End If
      Timing_ok = 0
      Timer1 = 4700
      If Timer_buffer > Seconds Then Start Timer1
      Here20:
      Return

      Timer_reset:
      Stop Timer1
      Flag = 0
      Timer_counter = 0
      Timer_buffer = 0
      Timing_ok = 0
      Seconds = 0
Return

'\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\

Flag_error:
      Hwstack = 605
      Call Test( "TRAVEL TIME ERR " , 1 , 1)
      L1 = &B01100000
      Enable Interrupts
      Enable Timer0
      Enable Ovf0
      Start Timer0
      $baud = 11364
      Enable Interrupts
      Enable Urxc
      Gosub Timer_reset
      'Set L1_l
      'Reset L1_l
      Do
         'Stop Watchdog
         Flag = 0
         Call_clears_bit = 1
         'Gosub Call_clears
         Gosub Timer_reset
         Seg_code = 6
         Gosub Code
         Gosub Check_rev
      Loop
Return

'\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\


Lock_error:
      Gosub Timer_reset
      L1 = &B01100000
      Call_clears_bit = 1
      Gosub Set_open_relay
      Call_clears_bit = 1
      Call Test( "LOCK ERROR        " , 1 , 1)
      Seg_code = 2
      Gosub Code
      Call_clears_bit = 1
      Lock_error_bit = 1
Goto Main

'\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\

S_stop_error:
      Gosub Timer_reset
      L1 = &B01100000
      Call_clears_bit = 1
      Gosub Set_open_relay
      Call_clears_bit = 1
      Call Test( "SERI STOP ERROR." , 1 , 1)
      Seg_code = 2
      Gosub Code
      Call_clears_bit = 1
      Lock_error_bit = 1
Goto Main

'\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\

Code:
      Wait 1

      S2 = 0
      If Seg_code = 1 Then
            Code_k1 = &B00111010                            'o
            Code_k2 = &B00011100                            'L
            Seg_ser = 41
      End If

      If Seg_code = 2 Then
            Code_k1 = &B00011100                            'L
            Code_k2 = &B10011110                            'E
            Seg_ser = 42
      End If

      If Seg_code = 3 Then
            Code_k1 = &B10001110                            'F
            Code_k2 = &B00111110                            'b
            Seg_ser = 43
      End If

      If Seg_code = 4 Then
            Code_k1 = &B00111010                            'o
            Code_k2 = &B10011110                            'E
            Seg_ser = 44
      End If

      If Seg_code = 5 Then
            Code_k1 = &B01111010                            'd
            Code_k2 = &B00111010                            'o
            Seg_ser = 45
      End If

      If Seg_code = 6 Then
            Code_k1 = &B10001110                            'F
            Code_k2 = &B10011110                            'E
            Seg_ser = 46
      End If

      If Seg_code = 7 Then
            Code_k1 = &B01111010                            'd
            Code_k2 = &B10011110                            'E
            Seg_ser = 47
      End If

      If Seg_code = 8 Then
            Code_k1 = &B01100110                            '4
            Code_k2 = &B00111110                            'b
            Seg_ser = 48
      End If

      If Seg_code = 9 Then
            Code_k1 = &B00111010                            'o
            Code_k2 = &B01101110                            'H
            Seg_ser = 49
      End If


      S1 = &B00000000
      Waitms 150
      S1 = Code_k1
      Waitms 800
      S1 = &B00000000
      Waitms 150
      S1 = Code_k2
      Waitms 800
      S1 = &B00000000
      Waitms 150
      Gosub 7seg
      Waitms 800


Return



'\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\



Check_programing:

         If En_flag = 1 Then
               En_flag = 0
               L1 = &B00100000
               S1 = &B10010010
               S2 = &B10010010
               Waitms 200
               Gosub Timer_reset
               Gosub Reading_data
               Page = 0
               Row = 1
               Call_clears_bit = 1
               Cls
               Locate 1 , 1
               Lcd "PROGRAMING:        "
               Do
                  If Program = 1 Then Exit Do
               Loop
               Goto Programing
         End If

Return

 '\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\


Setting:
'(
First_setting1 = 15
Waitms 5
Call Test( "LOADING DATA...." , 1 , 1)
'NOMERATOR:
'Second_door:
'THIRD_door:
For Komak1 = 1 To 16
Komak2 = Komak1 + 14
Komak3 = 0
Nomerator(komak1) = Komak2
Second_door(komak1) = Komak3
Third_door(komak1) = Komak3
Waitms 5
Next Komak1


'Service_limited:
Service_limited(1) = 0
Waitms 1
Service_limited(2) = 0
Waitms 1
Service_limited(3) = 0
Waitms 1
Service_limited(4) = 0
Waitms 1


'1-FLOOR NUMBER:
Aa(1) = 8
Waitms 1
'2-NOMERATOR
'3-DOOR SYSTEM:
Aa(3) = 0
Waitms 1
'4-BASE FLOOR:
Aa(4) = 1
Waitms 1
'5-PARK FLOOR:
Aa(5) = 0
Waitms 1
'6-MOVE TIME:
Aa(6) = 30
Waitms 1
'7-LIGHT TIME:
Aa(7) = 35
Waitms 1
'8-SERVICE TYPE:
Aa(8) = 0
Waitms 1
'9-DOOR PARK MODE:
Aa(9) = 0
Waitms 1
'10-RC DELAY:
Aa(10) = 1
Waitms 1
'11-LAST MESSAGE:
Aa(11) = 0
Waitms 1
'12-UP STOP TIME:
Aa(12) = 0
Waitms 1
'13-DOWN STOP TIME:
Aa(13) = 0
Waitms 1
'14-CLOSE DOOR TIME:
Aa(14) = 7
Waitms 1
'15-OPEN DOOR TIME:
Aa(15) = 3
Waitms 1
'16-SECOND DOOR:
Aa(16) = 0
Waitms 1
'17-THEIRD DOOR:
Aa(17) = 0
Waitms 1
'18-SERVICE LIMITE
'19-DOUBLEX:
Aa(19) = 0
Waitms 1

Cls
')
Return

'///////////////////////////////////////////////////////////////////////////////

Bache_halghe:
         Gosub Shasii_cancel
         Shasii_cancel_disable_bit = 1
         Waitms 50
         Shasii_haman_tabaghe_bit = 0
         Gosub Shasii_haman_tabaghe
         Gosub Magnet_display
         Gosub Check_rev
         Gosub Ck_final
         Gosub Check_programing
         If Jahat = 1 And Signal_flag = 0 Then
         Gosub Shasii_up
         Gosub Shasii_dn
         End If
         If Jahat = 0 And Signal_flag = 0 Then
         Gosub Shasii_dn
         Gosub Shasii_up
         End If
         If Signal_flag = 1 Then Call_clears_bit = 1
         If D_con = 1 Or Do1_bit = 1 Then Goto Main

Return

'///////////////////////////////////////////////////////////////////////////////


Door_mode_error:
      Do
            If D_con = 1 Then Goto Main
            Door_mode_byte = 0
            L1 = &B01100000                                 'LIGHT+DO =1
            Call Test( "DOOR BRIDGE ERR " , 1 , 1)
            Gosub Check_rev
            Gosub Timer_reset
            Call_clears_bit = 1
            Seg_code = 7
            Gosub Code
            Waitms 500                                      ' CHECK MU,MD=0
      Loop

'\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\

Shasii_haman_tabaghe:
      If Shasii_haman_tabaghe_bit = 1 Then Return

      If Sh_dakhel(level) = 0 Or Sh_kharej(level) = 0 Then
            Haman_tabaghe_counter = Haman_tabaghe_counter + 1
            If Haman_tabaghe_counter > 3 Then               'agar  bish az 5 bar shassi haman tabaghe zade shavad tavajoh nakarde o miravad
                   Shasii_haman_tabaghe_bit = 1
                   Haman_tabaghe_counter = 0
                   Dar_baz_ast = 0
                   Gosub Shasii_cancel
                   Door_mode_byte = 0
                   Goto Biroone_haman_tabaghe
            End If

            Shasii_haman_tabaghe_bit = 1
            Dar_baz_ast = 0
            Gosub Shasii_cancel
            Gosub Set_open_relay
            Goto Main
      End If

Biroone_haman_tabaghe:
Return

'\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\

Revesion_out:

      Wait 5
      Rev_off = 0
      Rev_light_counter = 0
      Dar_baz_ast = 0

      Call_clears_bit = 1
      Gosub Timer_reset
      Gosub Close_door
      Call_clears_bit = 1


      If Mu_in = 1 And Md_in = 1 Then
               Call Test( "Culibration     " , 1 , 1)
               Wait 1
               If Dls = 1 Or Uls = 1 Then Goto Biroone_rev_out_inja
               Sh_kharej(1) = 0
               Goto Bache_halghe
Biroone_rev_out_inja:
      End If


      If Dls = 1 And Uls = 0 Then
            If Uls = 1 And Dls = 1 Then Goto Back_uls_dls
            Call Test( "DLS IS ACTIVE   " , 1 , 1)
            Jahat = 0
            Waitms 300
            L1 = &B10111010
            Stop_emr_buffer = &B10111010

            Dorandaz = 1
            Mu = 2
            Md = 2
            Level = 1
            Gosub Magnet_display
            Gosub Check_stop_emr

            If Mu_in = 1 And Md_in = 1 And Rev = 0 Then
               Mu = 1
               Md = 1
               Level = 1
               Gosub 7seg

               L1 = &B10110010
               Waitms 500
               L1 = &B00100000                              ' JAHAT VA HIGH VA LOW BARDASHTE MISHAVD O BARAYE BAZ KARDANE DARB MIRAVAD

               Gosub Write_mu_md
               Gosub Timer_reset
               Gosub Magnet_display
               Gosub Set_open_relay
               If Rev = 0 Then Goto Main
            End If



            Do
               Call_clears_bit = 1
               Gosub Check_rev
               Gosub Check_mu
               Gosub Check_md
               Gosub 7seg
               Gosub Ck_sare_tabaghe
               Shasii_cancel_disable_bit = 1
               Gosub Check_stop_emr
               If Mu_in = 1 And Md_in = 1 Then
                     Gosub Mu_md_fard_zoj
                     If Md_fard = 0 Then Md = Md + 1
                     Gosub Stop_move
                     Goto Main
               End If
            Loop

      End If


      If Dls = 0 And Uls = 0 Then
            If Uls = 1 And Dls = 1 Then Goto Back_uls_dls
            Call Test( "SLOW DOWN        " , 1 , 1)
            Jahat = 0
            Waitms 300
            L1 = &B10111010
            Stop_emr_buffer = &B10111010


            Do
                  Call_clears_bit = 1
                  Gosub Check_rev
                  Gosub Check_mu
                  Gosub Check_md
                  Gosub Check_dls
                  Gosub Check_uls
                  Gosub 7seg
                  Gosub Ck_sare_tabaghe
                  Shasii_cancel_disable_bit = 1
                  Gosub Check_stop_emr
                  If Mu_in = 1 And Md_in = 1 Then
                        Gosub Mu_md_fard_zoj
                        If Md_fard = 0 Then Md = Md + 1
                        Gosub Stop_move
                        Goto Main
                  End If
            Loop
       End If


      If Uls = 1 And Dls = 0 Then
            If Uls = 1 And Dls = 1 Then Goto Back_uls_dls
            L1 = &B10111000
            Waitms 500
            L1 = &B10111001

            Stop_emr_buffer = &B10111001
            Call Test( "SLOW UP         " , 1 , 1)
            Gosub Timer_reset

            Do
                  Timer_buffer = Flag_time
                  Start Timer1
                  Flag = 1

                  Shasii_cancel_disable_bit = 1
                  Gosub Check_rev
                  Gosub Ck_sare_tabaghe
                  Gosub Check_uls
                  Gosub Check_stop_emr
                  If Mu_in = 1 And Mu_active = 0 Then
                     Gosub Timer_reset
                     Mu = Mu - 1
                     Mu_active = 1
                  End If
                  If Md_in = 1 And Md_active = 0 Then
                     Gosub Timer_reset
                     Md = Md - 1
                     Md_active = 1
                  End If
                  If Mu_in = 0 Then Mu_active = 0
                  If Md_in = 0 Then Md_active = 0
                  If Md = 0 Then Md = 1
                  If Mu = 0 Then Mu = 1
                  Gosub Write_mu_md
                  Gosub Magnet_display
            Loop

      End If
Back_uls_dls:

      Gosub Check_rev
      Gosub 7seg
      Gosub Timer_reset
      Gosub Close_door

      If Uls = 1 And Dls = 1 Then
            L1 = &B00100000
            Call Test( "ULS+DLS ERROR   " , 1 , 1)
            Gosub Check_rev
            Waitms 300
            If Uls = 1 And Dls = 1 Then Goto Back_uls_dls
       End If


'\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\


Shenasaii:

      If Uls = 1 And Dls = 1 Then Goto Back_uls_dls
      Mood = 0
      Gosub Check_rev
      Gosub 7seg
      Gosub Timer_reset
      Gosub Close_door

Shenasaii_biclose:
      If Uls = 1 And Dls = 1 Then Goto Back_uls_dls
      Gosub Ck_sare_tabaghe
      Gosub Check_dls
      Gosub Check_uls


      If Level = Aa(76) Then                                'LEVELE AKHARIN TABAGHE
              If Uls = 1 And Dls = 1 Then Goto Back_uls_dls
               'LET'S SLOW UP
               L1 = &B10110100
               Waitms 300
               L1 = &B10110101


               Stop_emr_buffer = &B10110101
               Call Test( "HIGH UP!        " , 1 , 1)
               Gosub Timer_reset

         Do
               If Uls = 1 And Dls = 1 Then Goto Back_uls_dls
               Timer_buffer = Flag_time
               Start Timer1
               Flag = 1

               Shasii_cancel_disable_bit = 1
               Gosub Ck_sare_tabaghe
               Gosub Check_rev
               Gosub Check_uls
               Gosub Check_stop_emr
               If Mu_in = 1 And Mu_active = 0 Then
                     Gosub Timer_reset
                     Mu = Mu + 1
                     Mu_active = 1
               End If
               If Md_in = 1 And Md_active = 0 Then
                     Gosub Timer_reset
                     Md = Md + 1
                     Md_active = 1
               End If
                     If Mu_in = 0 Then Mu_active = 0
                     If Md_in = 0 Then Md_active = 0
                     If Md = 0 Then Md = 1
                     If Mu = 0 Then Mu = 1
                     Gosub Write_mu_md
                     Gosub Magnet_display
         Loop
      End If


      'LET'S HIGH DN
      If Uls = 1 And Dls = 1 Then Goto Back_uls_dls
      L1 = &B10110100
      Waitms 300
      L1 = &B10110110

      Stop_emr_buffer = &B10110110
      Call Test( "HIGH DOWN!        " , 1 , 1)
      Gosub Timer_reset

      Do
            If Uls = 1 And Dls = 1 Then Goto Back_uls_dls
            Timer_buffer = Flag_time
            Start Timer1
            Flag = 1

            Shasii_cancel_disable_bit = 1
            Gosub Check_rev
            Gosub Ck_sare_tabaghe
            Gosub Check_dls
            Gosub Check_stop_emr
            If Mu_in = 1 And Mu_active = 0 Then
               Gosub Timer_reset
               Mu = Mu - 1
               Mu_active = 1
            End If
            If Md_in = 1 And Md_active = 0 Then
               Gosub Timer_reset
               Md = Md - 1
               Md_active = 1
            End If
            If Mu_in = 0 Then Mu_active = 0
            If Md_in = 0 Then Md_active = 0
            If Md = 0 Then Md = 1
            If Mu = 0 Then Mu = 1
            Gosub Write_mu_md
            Gosub Magnet_display
      Loop

'\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\

Shenasaii2:
      Return
      'CHECK BALATARIN VA PAIINTARIN TABAGHE
      If Lock_error_bit = 1 Then Return
      If Level = Aa(76) And Uls = 0 Then Goto Shenasaii
      If Level = 1 And Dls = 0 Then Goto Shenasaii
      Return

'\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\

Write_mu_md:
      Mu_rom = Mu
      Waitms 2
      Md_rom = Md
      Waitms 2
Return

'\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\


Ck_4bs:
'(
If Seconds = 1 And 4bs = 1 Then
Do
L1 = &B00100000
'L1 = &B11011111
Call_clears_bit = 1
Gosub Timer_reset
Call Test( "4BS ERROR        " , 1 , 1)
Gosub 7seg
Gosub Check_rev
Seg_code = 8
Gosub Code
Loop
End If
')
Return


'\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\



Programing:
Rev_light_counter = 0
If En_flag = 1 Then
En_flag = 0
Cls
Gosub 7seg
Gosub Magnet_display
Goto Main
End If

Seg_ser = 40

Gosub Check_page_row

If Up = 0 Then
Gosub Programing_up
Gosub Check_page_row
Waitms 250
End If
If Dn = 0 Then
Gosub Programing_dn
Gosub Check_page_row
Waitms 250
End If


      If Program = 0 Then
            Do
                  If Program = 1 Then Exit Do
                  If En_flag = 1 Then
                     En_flag = 0
                     Cls
                     Gosub 7seg
                     Gosub Magnet_display
                     Goto Main
                  End If
            Loop

            Gosub Programing_en
            Gosub Check_page_row
      End If

Goto Programing

Check_page_row:

      'Gosub Call_clears
      Call_clears_bit = 1
      Locate 1 , 1
      If Page = 0 Then Lcd "PROGRAMING:        "
      If Page = 30 Then Lcd "FLOOR SETTING:"

      For I = 310 To 460 Step 10
         If Page = I Then
            Lcd "FL"
            Komak4 = I - 300
            Komak5 = Komak4 / 10
            Lcd Komak5
            Lcd ":"
         End If
      Next I


      For I = 100 To 3200 Step 100
            Komak4 = 3010 + I
            If Page = Komak4 Then Lcd "CAR KEY:"
            Komak4 = 3020 + I
            If Page = Komak4 Then Lcd "HALL KEY:"
            Komak4 = 3030 + I
            If Page = Komak4 Then Lcd "NOMRATOR:"
            Komak4 = 3040 + I
            If Page = Komak4 Then Lcd "COLLECT TYPE:"
      Next I

      If Page = 20 Then Lcd "TIMER SETTING:"
      If Page = 10 Then Lcd "SETUP:           "
      If Page = 210 Then Lcd "CAR LIGHT:"
      If Page = 220 Then Lcd "PARK TIME:"
      If Page = 230 Then Lcd "DOOR CLOSE TIME:"
      If Page = 240 Then Lcd "DOOR OPEN TIME:"
      If Page = 250 Then Lcd "TRAVEL TIME:"
      If Page = 260 Then Lcd "UP STOP TIME:"
      If Page = 270 Then Lcd "DOWN STOP TIME:"
      If Page = 110 Then Lcd "MOTOR TYPE:"
      If Page = 120 Then Lcd "CAM DELAY:"
      If Page = 130 Then Lcd "BLINK SEGMENT:"
      If Page = 140 Then Lcd "DOOR PARK MODE:"
      If Page = 150 Then Lcd "PARK FLOOR:"
      If Page = 160 Then Lcd "FLOOR NUMBER:"
      If Page = 170 Then Lcd "DOOR MODE:"

      'Debounce Up , 0 , Programing_up , Sub
      'Debounce Dn , 0 , Programing_dn , Sub
      'Debounce Program , 0 , Programing_en , Sub


      If Row = 0 Then Row = 1
      Locate 2 , 1
      Lcd Row

      Lcd ":"

      If Page = 0 Then Gosub G0
      If Page = 30 Then Gosub G30

      For I = 10 To 160 Step 10
         Komak4 = 300 + I
         If Page = Komak4 Then Gosub G310
      Next I

      'If Page = 310 Then Gosub G310
      'If Page = 320 Then Gosub G310
      'If Page = 330 Then Gosub G310
      'If Page = 340 Then Gosub G310
      'If Page = 350 Then Gosub G310
      'If Page = 360 Then Gosub G310
      'If Page = 370 Then Gosub G310
      'If Page = 380 Then Gosub G310

      'CAR KEY:
      For I = 100 To 1600 Step 100
         Komak4 = 3010 + I
         Komak6 = 3020 + I
         If Page = Komak4 Or Page = Komak6 Then Gosub G3110
      Next I
      'If Page = 3110 Then Gosub G3110
      'If Page = 3210 Then Gosub G3110
      'If Page = 3310 Then Gosub G3110
      'If Page = 3410 Then Gosub G3110
      'If Page = 3510 Then Gosub G3110
      'If Page = 3610 Then Gosub G3110
      'If Page = 3710 Then Gosub G3110
      'If Page = 3810 Then Gosub G3110

      'HALL KEY:
      'If Page = 3120 Then Gosub G3110
      'If Page = 3220 Then Gosub G3110
      'If Page = 3320 Then Gosub G3110
      'If Page = 3420 Then Gosub G3110
      'If Page = 3520 Then Gosub G3110
      'If Page = 3620 Then Gosub G3110
      'If Page = 3720 Then Gosub G3110
      'If Page = 3820 Then Gosub G3110

      'NOMRATOR:
      For I = 100 To 1600 Step 100
         Komak4 = 3030 + I
         Komak5 = 3040 + I
         If Page = Komak4 Then Gosub G3130
         If Page = Komak5 Then Gosub G3140
      Next I
      'If Page = 3130 Then Gosub G3130
      'If Page = 3230 Then Gosub G3130
      'If Page = 3330 Then Gosub G3130
      'If Page = 3430 Then Gosub G3130
      'If Page = 3530 Then Gosub G3130
      'If Page = 3630 Then Gosub G3130
      'If Page = 3730 Then Gosub G3130
      'If Page = 3830 Then Gosub G3130


      If Page = 20 Then Gosub G20
      If Page = 10 Then Gosub G10
      If Page = 210 Then Gosub G210
      If Page = 220 Then Gosub G220
      If Page = 230 Then Gosub G230
      If Page = 240 Then Gosub G240
      If Page = 250 Then Gosub G250
      If Page = 260 Then Gosub G260
      If Page = 270 Then Gosub G270
      If Page = 110 Then Gosub G110
      If Page = 120 Then Gosub G120
      If Page = 130 Then Gosub G130
      If Page = 140 Then Gosub G140
      If Page = 150 Then Gosub G150
      If Page = 160 Then Gosub G160
      If Page = 170 Then Gosub G170


Return



Programing_dn:
Gosub Clear_lcd_line2
Row = Row - 1
If Row = 0 Then Row = 1
Gosub Check_mammin
Return

Programing_up:
Gosub Clear_lcd_line2
Row = Row + 1
Gosub Check_mammin
Return


Programing_en:
Gosub Clear_lcd_line1
Gosub Clear_lcd_line2
If Page = 30 And Row = 17 Then
Gosub Back_page
Waitms 500
Goto Programing
End If


If Page => 310 And Page =< 460 And Row = 5 Then
Gosub Back_page
Waitms 500
Goto Programing
End If


'CAR KEY:
Gosub Dahgan
If Dahgan = 1 And Row = 3 And Tedad = 1 Then
Gosub Back_page
Waitms 500
Goto Programing
End If

'HALL KEY

Gosub Dahgan
If Dahgan = 2 And Row = 3 And Tedad = 1 Then
Gosub Back_page
Waitms 500
Goto Programing
End If

'NOMRATOR:

Gosub Dahgan
If Dahgan = 3 And Row = 29 And Tedad = 1 Then
Gosub Back_page
Waitms 500
Goto Programing
End If

'COLLECTYPE:

Gosub Dahgan
If Dahgan = 4 And Row = 4 And Tedad = 1 Then
Gosub Back_page
Waitms 500
Goto Programing
End If


If Page = 20 And Row = 8 Then
Gosub Back_page
Waitms 500
Goto Programing
End If

If Page = 10 And Row = 8 Then
Gosub Back_page
Waitms 500
Goto Programing
End If

If Page = 210 And Row = 14 Then
Gosub Back_page
Waitms 500
Goto Programing
End If

If Page = 220 And Row = 14 Then
Gosub Back_page
Waitms 500
Goto Programing
End If

If Page = 230 And Row = 14 Then
Gosub Back_page
Waitms 500
Goto Programing
End If

If Page = 240 And Row = 14 Then
Gosub Back_page
Waitms 500
Goto Programing
End If

If Page = 250 And Row = 9 Then
Gosub Back_page
Waitms 500
Goto Programing
End If

If Page = 260 And Row = 12 Then
Gosub Back_page
Waitms 500
Goto Programing
End If

If Page = 270 And Row = 12 Then
Gosub Back_page
Waitms 500
Goto Programing
End If

If Page = 110 And Row = 6 Then
Gosub Back_page
Waitms 500
Goto Programing
End If

If Page = 120 And Row = 3 Then
Gosub Back_page
Waitms 500
Goto Programing
End If

If Page = 130 And Row = 3 Then
Gosub Back_page
Waitms 500
Goto Programing
End If

If Page = 140 And Row = 3 Then
Gosub Back_page
Waitms 500
Goto Programing
End If

If Page = 150 And Row = 18 Then
Gosub Back_page
Waitms 500
Goto Programing
End If

If Page = 160 And Row = 17 Then
Gosub Back_page
Waitms 500
Goto Programing
End If

If Page = 170 And Row = 4 Then
Gosub Back_page
Waitms 500
Goto Programing
End If



'CAR KEY SAVING...
Address = Page / 10
Gosub Dahgan
'TEDAD=1 ==> 4RAGHAMI AST
If Tedad = 1 Then
Gosub Writing
'BARAYE BAZGASHTE SAHIH PAS AZ SAVE KARDAN
Komak4 = Page + Row
Page = Komak4 * 10
Row = Dahgan
Return
End If
'If Page = 3110 Then Gosub Writing
'If Page = 3210 Then Gosub Writing
'If Page = 3310 Then Gosub Writing
'If Page = 3410 Then Gosub Writing
'If Page = 3510 Then Gosub Writing
'If Page = 3610 Then Gosub Writing
'If Page = 3710 Then Gosub Writing
'If Page = 3810 Then Gosub Writing

'SAVING TIMES & SAVING SETTINGS:
If Sadgan = 2 Or Sadgan = 1 Then
Gosub Writing
'BARAYE BAZGASHTE SAHIH PAS AZ SAVE KARDAN
'Komak4 = Page + Row
'Page = Komak4 * 10
Row = Dahgan
Return
End If





Komak4 = Page + Row
Page = Komak4 * 10
Row = 1

'RELOADING:

J = 1
For I = 100 To 1600 Step 100
Komak4 = 3010 + I
Gosub Komaki1
Next I

'If Page = 3110 Then Row = Aa(1)
'If Page = 3210 Then Row = Aa(2)
'If Page = 3310 Then Row = Aa(3)
'If Page = 3410 Then Row = Aa(4)
'If Page = 3510 Then Row = Aa(5)
'If Page = 3610 Then Row = Aa(6)
'If Page = 3710 Then Row = Aa(7)
'If Page = 3810 Then Row = Aa(8)

J = 17
For I = 100 To 1600 Step 100
Komak4 = 3020 + I
Gosub Komaki1
Next I

'If Page = 3120 Then Row = Aa(17)
'If Page = 3220 Then Row = Aa(18)
'If Page = 3320 Then Row = Aa(19)
'If Page = 3420 Then Row = Aa(20)
'If Page = 3520 Then Row = Aa(21)
'If Page = 3620 Then Row = Aa(22)
'If Page = 3720 Then Row = Aa(23)
'If Page = 3820 Then Row = Aa(24)

J = 35
For I = 100 To 3200 Step 100
Komak4 = 3030 + I
Gosub Komaki1
Next I

'If Page = 3130 Then Row = Aa(35)
'If Page = 3230 Then Row = Aa(36)
'If Page = 3330 Then Row = Aa(37)
'If Page = 3430 Then Row = Aa(38)
'If Page = 3530 Then Row = Aa(39)
'If Page = 3630 Then Row = Aa(40)
'If Page = 3730 Then Row = Aa(41)
'If Page = 3830 Then Row = Aa(42)

      J = 61
         For I = 10 To 70 Step 10
         Komak4 = 200 + I
         Gosub Komaki1
      Next I

'If Page = 210 Then Row = Aa(61)
'If Page = 220 Then Row = Aa(62)
'If Page = 230 Then Row = Aa(63)
'If Page = 240 Then Row = Aa(64)
'If Page = 250 Then Row = Aa(65)
'If Page = 260 Then Row = Aa(66)
'If Page = 270 Then Row = Aa(67)

J = 71
For I = 10 To 70 Step 10
Komak4 = 100 + I
Gosub Komaki1
Next I

'If Page = 110 Then Row = Aa(71)
'If Page = 120 Then Row = Aa(72)
'If Page = 130 Then Row = Aa(73)
'If Page = 140 Then Row = Aa(74)
'If Page = 150 Then Row = Aa(75)
'If Page = 160 Then Row = Aa(76)

J = 84
For I = 100 To 1600 Step 100
Komak4 = 3040 + I
Gosub Komaki1
Next I

Return

Komaki1:
If Page = Komak4 Then Row = Aa(j)
J = J + 1
Return


G0:
If Row = 3 Then Lcd "FLOOR SETTING"
If Row = 2 Then Lcd "TIMER SETTING"
If Row = 1 Then Lcd "SETUP"
Return

'G30:
'If Row = 1 Then Lcd "FL1"
'If Row = 2 Then Lcd "FL2"
'If Row = 3 Then Lcd "FL3"
'If Row = 4 Then Lcd "FL4"
'If Row = 5 Then Lcd "FL5"
'If Row = 6 Then Lcd "FL6"
'If Row = 7 Then Lcd "FL7"
'If Row = 8 Then Lcd "FL8"
'If Row = 9 Then Lcd "(BACK)"
'Return


G310:
If Row = 1 Then Lcd "CAR KEY"
If Row = 2 Then Lcd "HALL KEY"
If Row = 3 Then Lcd "NOMRATOR"
If Row = 4 Then Lcd "COLLECT TYPE"
If Row = 5 Then Lcd "(BACK)"
Return

'G3110:
'If Row = 1 Then Lcd "ENABLE"
'If Row = 2 Then Lcd "DISABLE"
'If Row = 3 Then Lcd "(BACK)"
'Return

G3140:
   If Row = 4 Then
      Lcd "(BACK)"
      Return
   End If
If Row = 1 Then Lcd "FULL"
If Row = 2 Then Lcd "UP"
If Row = 3 Then Lcd "DOWN"
Lcd "COLLECT"
Return

G3130:
      If Row = 1 Then Lcd "-1"
      If Row = 2 Then Lcd "-2"
      If Row = 3 Then Lcd "-3"
      If Row = 4 Then Lcd "-4"
      If Row = 5 Then Lcd "P"
      If Row = 6 Then Lcd "b"
      If Row = 7 Then Lcd "G"
      If Row = 29 Then
      Lcd "(BACK)"
Return
End If

If Row > 7 Then
Komak5 = Row - 8
Lcd Komak5
End If



'If Row = 8 Then Lcd "0"
'If Row = 9 Then Lcd "1"
'If Row = 10 Then Lcd "2"
'If Row = 11 Then Lcd "3"
'If Row = 12 Then Lcd "4"
'If Row = 13 Then Lcd "5"
'If Row = 14 Then Lcd "6"
'If Row = 15 Then Lcd "7"
'If Row = 16 Then Lcd "8"
'If Row = 17 Then Lcd "(BACK)"
Return

G20:
If Row = 1 Then Lcd "CAR LIGHT"
If Row = 2 Then Lcd "PARK TIME"
If Row = 3 Then Lcd "DOOR CLOSE TIME"
If Row = 4 Then Lcd "DOOR OPEN TIME"
If Row = 5 Then Lcd "TRAVEL TIME"
If Row = 6 Then Lcd "UP STOP TIME"
If Row = 7 Then Lcd "DOWN STOP TIME"
If Row = 8 Then Lcd "(BACK)"
Return


G10:
If Row = 1 Then Lcd "MOTOR TYPE"
If Row = 2 Then Lcd "CAM DELAY"
If Row = 3 Then Lcd "BLINK SEGMENT"
If Row = 4 Then Lcd "DOOR PARK MODE"
If Row = 5 Then Lcd "PARK FLOOR"
If Row = 6 Then Lcd "FLOOR NUMBER"
If Row = 7 Then Lcd "DOOR MODE"
If Row = 8 Then Lcd "(BACK)"
Return

'G210:
'If Row = 1 Then Lcd "30S"
'If Row = 2 Then Lcd "40S"
'If Row = 3 Then Lcd "50S"
'If Row = 4 Then Lcd "60S"
'If Row = 5 Then Lcd "70S"
'If Row = 6 Then Lcd "80S"
'If Row = 7 Then Lcd "90S"
'If Row = 8 Then Lcd "100S"
'If Row = 9 Then Lcd "120S"
'If Row = 10 Then Lcd "140S"
'If Row = 11 Then Lcd "160S"
'If Row = 12 Then Lcd "180S"
'If Row = 13 Then Lcd "200S"
'If Row = 14 Then Lcd "(BACK)"
'Return

'G210:
'If Row < 9 Then Komak5 = Row + 2
'If Row > 8 Then
'Komak4 = Row - 6
'Komak5 = Komak5 + Row
'End If
'Meghdar1 = Lookupstr(row , M_data_prog)
'Lcd Meghdar1
'Lcd "0S"
'If Row = 14 Then Lcd "(BACK)"
'Return


G220:
If Row = 14 Then
Lcd "(BACK)"
Return
End If
If Row < 6 Then Komak5 = Row + 5
If Row > 5 Then Komak5 = Row * 2
If Row = 13 Then Komak5 = 25
Lcd Komak5
Lcd "0S"
'If Row = 1 Then Lcd "60S"
'If Row = 2 Then Lcd "70S"
'If Row = 3 Then Lcd "80S"
'If Row = 4 Then Lcd "90S"
'If Row = 5 Then Lcd "100S"
'If Row = 6 Then Lcd "120S"
'If Row = 7 Then Lcd "140S"
'If Row = 8 Then Lcd "160S"
'If Row = 9 Then Lcd "180S"
'If Row = 10 Then Lcd "200S"
'If Row = 11 Then Lcd "220S"
'If Row = 12 Then Lcd "240S"
'If Row = 13 Then Lcd "250S"
'If Row = 14 Then Lcd "(BACK)"
Return

'G230:
'If Row = 1 Then Lcd "3S"
'If Row = 2 Then Lcd "4S"
'If Row = 3 Then Lcd "5S"
'If Row = 4 Then Lcd "6S"
'If Row = 5 Then Lcd "7S"
'If Row = 6 Then Lcd "8S"
'If Row = 7 Then Lcd "9S"
'If Row = 8 Then Lcd "10S"
'If Row = 9 Then Lcd "12S"
'If Row = 10 Then Lcd "14S"
'If Row = 11 Then Lcd "16S"
'If Row = 12 Then Lcd "18S"
'If Row = 13 Then Lcd "20S"
'If Row = 14 Then Lcd "(BACK)"
'Return
'
G210:
G230:
G240:
If Row = 14 Then
Lcd "(BACK)"
Return
End If
If Row < 9 Then Komak5 = Row + 2
If Row > 8 Then
Komak4 = Row - 6
Komak5 = Row + Komak4
End If
Lcd Komak5
If Page = 210 Then
Lcd "0S"
Return
End If
Lcd "S"
'If Row = 1 Then Lcd "3S"
'If Row = 2 Then Lcd "4S"
'If Row = 3 Then Lcd "5S"
'If Row = 4 Then Lcd "6S"
'If Row = 5 Then Lcd "7S"
'If Row = 6 Then Lcd "8S"
'If Row = 7 Then Lcd "9S"
'If Row = 8 Then Lcd "10S"
'If Row = 9 Then Lcd "12S"
'If Row = 10 Then Lcd "14S"
'If Row = 11 Then Lcd "16S"
'If Row = 12 Then Lcd "18S"
'If Row = 13 Then Lcd "20S"
Return

G250:
If Row = 1 Then Lcd "AUTO"
If Row = 2 Then Lcd "16S"
If Row = 3 Then Lcd "19S"
If Row = 4 Then Lcd "22S"
If Row = 5 Then Lcd "25S"
If Row = 6 Then Lcd "28S"
If Row = 7 Then Lcd "31S"
If Row = 8 Then Lcd "34S"
If Row = 9 Then Lcd "(BACK)"
Return

'G260:
'If Row = 1 Then Lcd "0MS"
'If Row = 2 Then Lcd "100MS"
'If Row = 3 Then Lcd "200MS"
'If Row = 4 Then Lcd "300MS"
'If Row = 5 Then Lcd "400MS"
'If Row = 6 Then Lcd "500MS"
'If Row = 7 Then Lcd "600MS"
'If Row = 8 Then Lcd "700MS"
'If Row = 9 Then Lcd "800MS"
'If Row = 10 Then Lcd "900MS"
'If Row = 11 Then Lcd "1000MS"
'If Row = 12 Then Lcd "(BACK)"
'Return

G260:
G270:
If Row = 12 Then
Lcd "(BACK)"
Return
End If
Komak5 = Row - 1
Lcd Komak5
Lcd "00MS"
'If Row = 1 Then Lcd "0MS"
'If Row = 2 Then Lcd "100MS"
'If Row = 3 Then Lcd "200MS"
'If Row = 4 Then Lcd "300MS"
'If Row = 5 Then Lcd "400MS"
'If Row = 6 Then Lcd "500MS"
'If Row = 7 Then Lcd "600MS"
'If Row = 8 Then Lcd "700MS"
'If Row = 9 Then Lcd "800MS"
'If Row = 10 Then Lcd "900MS"
'If Row = 11 Then Lcd "1000MS"
Return

G110:
If Row = 1 Then Lcd "-"
If Row = 2 Then Lcd "3VF-2SPEED"
If Row = 3 Then Lcd "-"
If Row = 4 Then Lcd "-"
If Row = 5 Then Lcd "-"
If Row = 6 Then Lcd "(BACK)"
Return

'G120:
'If Row = 1 Then Lcd "ENABLE"
'If Row = 2 Then Lcd "DISABLE"
'If Row = 3 Then Lcd "(BACK)"
'Return

G3110:
G120:
G130:
If Row = 1 Then Lcd "ENABLE"
If Row = 2 Then Lcd "DISABLE"
If Row = 3 Then Lcd "(BACK)"
Return

G140:
If Row = 1 Then Lcd "DOOR OPEN"
If Row = 2 Then Lcd "DOOR CLOSE"
If Row = 3 Then Lcd "(BACK)"
Return

'G150:
'If Row = 1 Then Lcd "DISABLE"
'If Row = 2 Then Lcd "FL1"
'If Row = 3 Then Lcd "FL2"
'If Row = 4 Then Lcd "FL3"
'If Row = 5 Then Lcd "FL4"
'If Row = 6 Then Lcd "FL5"
'If Row = 7 Then Lcd "FL6"
'If Row = 8 Then Lcd "FL7"
'If Row = 9 Then Lcd "FL8"
'If Row = 10 Then Lcd "(BACK)"
'Return

G160:
G150:
G30:
Komak5 = Row
      If Page = 150 And Komak5 = 1 Then
         Lcd "DISABLE"
         Return
      End If

      If Page = 150 Then Komak5 = Row - 1

      If Komak5 = 17 Then
         Lcd "(BACK)"
         Return
      End If
Lcd "FL"
Lcd Komak5
Park_floor = Komak5
Aa(75) = Park_floor
'If Komak5 = 1 Then Lcd "FL1"
'If Komak5 = 2 Then Lcd "FL2"
'If Komak5 = 3 Then Lcd "FL3"
'If Komak5 = 4 Then Lcd "FL4"
'If Komak5 = 5 Then Lcd "FL5"
'If Komak5 = 6 Then Lcd "FL6"
'If Komak5 = 7 Then Lcd "FL7"
'If Komak5 = 8 Then Lcd "FL8"
Return

G170:
If Row = 1 Then Lcd "SEMI AUTO"
If Row = 2 Then Lcd "AUTOMATIC"
If Row = 3 Then Lcd "3PHASE"
If Row = 4 Then Lcd "(BACK)"
Return


Check_mammin:

For I = 100 To 1600 Step 100
Komak4 = 3010 + I
Komak5 = Komak4 + 10
If Page = 0 Or Page = 120 Or Page = 130 Or Page = 140 Or Page = Komak4 Or Page = Komak5 Then
If Row = 4 Then Row = 3
Return
End If
Next I

'If Page = 0 Or Page = 120 Or Page = 130 Or Page = 140 Or Page = 3110 Or Page = 3210 Or Page = 3310 Or Page = 3410 Or Page = 3510 Or Page = 3610 Or Page = 3710 Or Page = 3810 Or Page = 3120 Or Page = 3220 Or Page = 3320 Or Page = 3420 Or Page = 3520 Or Page = 3620 Or Page = 3720 Or Page = 3820 Then
'If Row = 4 Then Row = 3
'Return
'End If

'NOMRATOR
For I = 100 To 1600 Step 100
Komak4 = 3030 + I
If Page = Komak4 Then
If Row = 30 Then Row = 29
Return
End If
Next I

'COLLECTYPE
For I = 100 To 3200 Step 100
Komak4 = 3040 + I
If Page = Komak4 Then
If Row = 5 Then Row = 4
Return
End If
Next I

'If Page = 3130 Or Page = 3230 Or Page = 3330 Or Page = 3430 Or Page = 3530 Or Page = 3630 Or Page = 3730 Or Page = 3830 Then
'If Row = 18 Then Row = 17
'Return
'End If

'If Page = 310 Then
'If Row = 6 Then Row = 5
'Return
'End If

If Page => 310 And Page < 470 Then
If Row = 6 Then Row = 5
Return
End If

If Page = 20 Or Page = 10 Then
If Row = 9 Then Row = 8
Return
End If

If Page = 170 Then
If Row = 5 Then Row = 4
Return
End If


If Page = 210 Or Page = 220 Or Page = 230 Or Page = 240 Then
If Row = 15 Then Row = 14
Return
End If

If Page = 250 Then
If Row = 10 Then Row = 9
Return
End If

If Page = 260 Or Page = 270 Then
If Row = 13 Then Row = 12
Return
End If

If Page = 110 Then
If Row = 7 Then Row = 6
Return
End If

If Page = 30 Or Page = 160 Then
If Row = 18 Then Row = 17
Return
End If

If Page = 150 Then
If Row = 19 Then Row = 18
Return
End If

Return


Back_page:
'If Page > 310 And Page < 400 Then Page = 310
Komak4 = Page / 10
Komak5 = Komak4 / 10
Page = Komak5 * 10
Row = Komak4 - Page
'FOR 16 LEVEL:
If Page = 40 Then
Page = 30
Row = Row + 10
End If
Gosub Clear_lcd_line1
Return

Clear_lcd_line1:
Locate 1 , 1
Lcd "                 "
Locate 1 , 1
Return

Clear_lcd_line2:
Locate 2 , 1
Lcd "                 "
Locate 2 , 1
Return

Dahgan:
Tedad = 0
Komak4 = Page / 10
Sadgan = Komak4 / 10
If Komak4 > 99 Then Tedad = 1
Komak5 = Komak4 / 10
Komak5 = Komak5 * 10
Dahgan = Komak4 - Komak5
Return

Writing:
Writeeeprom Row , Address
Waitms 20
Cls
New_data:
Lcd "SAVING NEW DATA."
Waitms 50
For Komak4 = 1 To 16
Deflcdchar 0 , 31 , 31 , 31 , 31 , 31 , 31 , 31 , 31
Locate 2 , Komak4
Lcd Chr(0)
Waitms 50
Next Komak1
Waitms 100
Cls
Gosub Back_page
If Tedad = 1 Then Gosub Back_page
Gosub Reading_data
Return



Reading_data:


J = 70
'SETTING
For I = 11 To 17
J = J + 1
Gosub Readeeprom1
If Aa(j) > 20 Then Aa(j) = 1
Waitms 1
Next I

'TOW SPEED:
Readeeprom Aa(71) , 11
If Aa(71) > 20 Then
Aa(71) = 2
Waitms 1
End If


'FLOOR NUMBER
'CHON ASANSOOR 1 TABAGHE NADARIIM
If Aa(76) = 1 Then Aa(76) = 6


J = 0
'CAR KEY
For I = 311 To 461 Step 10
J = J + 1
Gosub Readeeprom1
If Aa(j) > 2 Then Aa(j) = 1
'FOR DIABLE FLOORS UPPER LEVEL
If J > Aa(76) Then Aa(j) = 2
Waitms 1
Next I


'HALL KEY
For I = 312 To 462 Step 10
J = J + 1
Gosub Readeeprom1
If Aa(j) > 2 Then Aa(j) = 1
'FOR DIABLE FLOORS UPPER LEVEL
Komak4 = J - 16
If Komak4 > Aa(76) Then Aa(j) = 2
Waitms 1
Next I


J = 34
Komak7 = 7
'NOMRATOR
For I = 313 To 463 Step 10
J = J + 1
Komak7 = Komak7 + 1
Gosub Readeeprom1
If Aa(j) > 50 Then Aa(j) = Komak7
Waitms 1
Next I
'P
I = 313 : J = 35
Gosub Readeeprom1
If Aa(35) > 50 Then Aa(j) = 5


J = 83
'COLLECT TYPE
For I = 314 To 464 Step 10
J = J + 1
Gosub Readeeprom1
If Aa(j) > 20 Then Aa(j) = 3
Waitms 1
Next I



For J = 61 To 67
I = J - 40
Gosub Readeeprom1
If Aa(j) > 20 Then Aa(j) = 1
Next J
''CAR LIGHT TIME
'Readeeprom Aa(61) , 21
'If Aa(61) > 20 Then Aa(61) = 1
''PARK TIME
'Readeeprom Aa(62) , 22
'If Aa(62) > 20 Then Aa(62) = 1
'DOOR CLOSE TIME
Readeeprom Aa(63) , 23
If Aa(63) > 20 Then Aa(63) = 7
'DOOR OPEN TIME
Readeeprom Aa(64) , 24
If Aa(64) > 20 Then Aa(64) = 5
''FLAG TIME
'Readeeprom Aa(65) , 25
'If Aa(65) > 20 Then Aa(65) = 1
''UP STOP TIME
'Readeeprom Aa(66) , 26
'If Aa(66) > 20 Then Aa(66) = 1
''DOWN DTOP TIME
'Readeeprom Aa(67) , 27
'If Aa(67) > 20 Then Aa(67) = 1

'FLAG TIME AUTO
                        If Aa(65) = 1 Then
                        Komak5 = Aa(76) - 1
                        Komak4 = Komak5 * 3
                        Flag_time = Komak4 + 10
                        End If

                        'FLAG TIME MANUAL
                        If Aa(65) > 1 And Aa(65) < 8 Then
                        Komak4 = Aa(65) * 3
                        Flag_time = Komak4 + 9
                        End If
                        If Aa(65) = 8 Then Flag_time = 60

'Readeeprom Mu , 78
'Readeeprom Md , 79
'
'If Mu = 0 Or Mu = 255 Then Mu = 1
'If Md = 0 Or Md = 255 Then Md = 1

Return

'///////////////////////////////////////////////////////////////////////////////

Readeeprom1:
   Readeeprom Aa(j) , I
Return

'///////////////////////////////////////////////////////////////////////////////

M_data:
   Data "0 " , "1 " , "2 " , "3 " , "4 " , "5 " , "6 " , "7 " , "8 " , "9 " , "10" , "11" , "12" , "13" , "14" , "15" , "16" , "17" , "18" , "19" , "20" , "21" , "22" , "23" , "24" , "25" , "26" , "27" , "28" , "29" , "30" , "31" , "32" , "33" , "34" , "35" , "36" , "37" , "38" , "39" , "40" , "41" , "42" , "43" , "44" , "45" , "46" , "47" , "48" , "49" , "50" , "51"


'///////////////////////////////////////////////////////////////////////////////

Tt:

      Signal_word = 0
      Signal_flag = 0

      If Ser_con2 = 1 Then Udr_k1 = Udr
      If Ser_con2 = 2 Then Udr_k2 = Udr
      If Ser_con2 = 3 Then Udr_k3 = Udr
      If Ser_con2 = 4 Then Udr_k4 = Udr
      If Ser_con2 = 5 Then Udr_k5 = Udr
      If Ser_con2 = 6 Then Udr_k6 = Udr
      If Ser_con2 = 7 Then Udr_k7 = Udr

Return

'///////////////////////////////////////////////////////////////////////////////


Ck_final:
      If Final = 1 Then
      Do
      Call Test( "FINAL GAVERNOR.  " , 1 , 1)
      Gosub Check_programing
      Call_clears_bit = 1
      L1 = &B01100000
      Gosub 7seg
      Gosub Timer_reset
      If Final = 0 Then Goto Main
      Loop
      End If
Return

'\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\

Set_open_relay:

      If Dar_baz_ast = 1 Then Return
      Call Test( "OPEN DOOR       " , 1 , 1)

      L1 = &B01100000
      Gosub Timer_reset
      Komak4 = Aa(64) + 2
      Timer_buffer = Komak4
      Start Timer1

      Do
            Gosub Shasii_cancel
            Shasii_cancel_disable_bit = 1
            Waitms 50
            'AGAR REV BASHIM VA JAHAT GEREFTE SHAVAD, KHAREJ MISHAVAD:
            If Rev = 1 And Close_err_flag = 0 Then
            If Rev = 1 And Ru = 0 And Us = 0 Then Return
            If Rev = 1 And Rd = 0 And Ds = 0 Then Return
            End If

            If Dc_bit = 0 And Seconds > 2 Then
               Gosub Timer_reset
               Exit Do
            End If
            If Timing_ok = 1 Then
               Gosub Timer_reset
               Exit Do
            End If
      Loop

      L1 = &B00100000

      'AGAR DAR REV BASHIM VA JAHAT GIRAD,KHAREJ SHAVAD.
      'BEJOZ DAR REV:
      If L_con = 0 And Mood <> 1 Then Goto Opening_error    'Chech If Door Is Open
      Dar_baz_ast = 1
Return

'\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\

Set_open_relay_bi_lcd:
         If Dar_baz_ast = 1 Then Return
         L1 = &B01100000
         Gosub Timer_reset
         Komak4 = Aa(64) + 2
         Timer_buffer = Komak4
         Start Timer1

         Do
               Gosub Shasii_cancel
               Shasii_cancel_disable_bit = 1
               Waitms 50
               'AGAR REV BASHIM VA JAHAT GEREFTE SHAVAD, KHAREJ MISHAVAD:
               If Rev = 1 And Close_err_flag = 0 Then
               If Rev = 1 And Ru = 0 And Us = 0 Then Return
               If Rev = 1 And Rd = 0 And Ds = 0 Then Return
               End If

               If Dc_bit = 0 And Seconds > 2 Then
                     Gosub Timer_reset
                     Goto Pain_inja_boro
                     End If
                     If Timing_ok = 1 Then
                     Gosub Timer_reset
                     Goto Pain_inja_boro
               End If
         Loop
Pain_inja_boro:
         L1 = &B00100000
         'AGAR DAR REV BASHIM VA JAHAT GIRAD,KHAREJ SHAVAD.
         'BEJOZ DAR REV:
         If L_con = 0 And Mood <> 1 Then Goto Opening_error 'Chech If Door Is Open
         Dar_baz_ast = 1
Return
