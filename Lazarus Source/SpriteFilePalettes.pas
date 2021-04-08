  const
   //Palette format is a chain of VDU commands:
   //VDU 19,col,mode,R,G,B
   //mode = $10 : redefine col
   //mode = $18 : redefine border col
   //mode = $19 : redefine cursor col
   ColourPalette2: array[0..$B] of Byte=($13,$00,$10,$FF,$FF,$FF,
                                         $13,$01,$10,$00,$00,$00);
   ColourPalette4: array[0..$17] of Byte=($13,$00,$10,$FF,$FF,$FF,
                                          $13,$01,$10,$BB,$BB,$BB,
                                          $13,$02,$10,$77,$77,$77,
                                          $13,$03,$10,$00,$00,$00);
   ColourPalette16: array[0..$77] of Byte=($13,$00,$10,$FF,$FF,$FF,
                                           $13,$01,$10,$DD,$DD,$DD,
                                           $13,$02,$10,$BB,$BB,$BB,
                                           $13,$03,$10,$99,$99,$99,
                                           $13,$04,$10,$77,$77,$77,
                                           $13,$05,$10,$55,$55,$55,
                                           $13,$06,$10,$33,$33,$33,
                                           $13,$07,$10,$00,$00,$00,
                                           $13,$08,$10,$00,$44,$99,
                                           $13,$09,$10,$EE,$EE,$00,
                                           $13,$0A,$10,$00,$CC,$00,
                                           $13,$0B,$10,$DD,$00,$00,
                                           $13,$0C,$10,$EE,$EE,$BB,
                                           $13,$0D,$10,$55,$88,$00,
                                           $13,$0E,$10,$FF,$BB,$00,
                                           $13,$0F,$10,$00,$BB,$FF,
                                           $13,$00,$18,$00,$00,$00,
                                           $13,$01,$19,$00,$FF,$FF,
                                           $13,$02,$19,$00,$00,$99,
                                           $13,$03,$19,$FF,$00,$00);
   ColourPalette256: array[0..$5FF] of Byte=($13,$00,$10,$00,$00,$00,
                                             $13,$01,$10,$11,$11,$11,
                                             $13,$02,$10,$22,$22,$22,
                                             $13,$03,$10,$33,$33,$33,
                                             $13,$04,$10,$44,$00,$00,
                                             $13,$05,$10,$55,$11,$11,
                                             $13,$06,$10,$66,$22,$22,
                                             $13,$07,$10,$77,$33,$33,
                                             $13,$08,$10,$00,$00,$44,
                                             $13,$09,$10,$11,$11,$55,
                                             $13,$0A,$10,$22,$22,$66,
                                             $13,$0B,$10,$33,$33,$77,
                                             $13,$0C,$10,$44,$00,$44,
                                             $13,$0D,$10,$55,$11,$55,
                                             $13,$0E,$10,$66,$22,$66,
                                             $13,$0F,$10,$77,$33,$77,
                                             $13,$10,$10,$88,$00,$00,
                                             $13,$11,$10,$99,$11,$11,
                                             $13,$12,$10,$AA,$22,$22,
                                             $13,$13,$10,$BB,$33,$33,
                                             $13,$14,$10,$CC,$00,$00,
                                             $13,$15,$10,$DD,$11,$11,
                                             $13,$16,$10,$EE,$22,$22,
                                             $13,$17,$10,$FF,$33,$33,
                                             $13,$18,$10,$88,$00,$44,
                                             $13,$19,$10,$99,$11,$55,
                                             $13,$1A,$10,$AA,$22,$66,
                                             $13,$1B,$10,$BB,$33,$77,
                                             $13,$1C,$10,$CC,$00,$44,
                                             $13,$1D,$10,$DD,$11,$55,
                                             $13,$1E,$10,$EE,$22,$66,
                                             $13,$1F,$10,$FF,$33,$77,
                                             $13,$20,$10,$00,$44,$00,
                                             $13,$21,$10,$11,$55,$11,
                                             $13,$22,$10,$22,$66,$22,
                                             $13,$23,$10,$33,$77,$33,
                                             $13,$24,$10,$44,$44,$00,
                                             $13,$25,$10,$55,$55,$11,
                                             $13,$26,$10,$66,$66,$22,
                                             $13,$27,$10,$77,$77,$33,
                                             $13,$28,$10,$00,$44,$44,
                                             $13,$29,$10,$11,$55,$55,
                                             $13,$2A,$10,$22,$66,$66,
                                             $13,$2B,$10,$33,$77,$77,
                                             $13,$2C,$10,$44,$44,$44,
                                             $13,$2D,$10,$55,$55,$55,
                                             $13,$2E,$10,$66,$66,$66,
                                             $13,$2F,$10,$77,$77,$77,
                                             $13,$30,$10,$88,$44,$00,
                                             $13,$31,$10,$99,$55,$11,
                                             $13,$32,$10,$AA,$66,$22,
                                             $13,$33,$10,$BB,$77,$33,
                                             $13,$34,$10,$CC,$44,$00,
                                             $13,$35,$10,$DD,$55,$11,
                                             $13,$36,$10,$EE,$66,$22,
                                             $13,$37,$10,$FF,$77,$33,
                                             $13,$38,$10,$88,$44,$44,
                                             $13,$39,$10,$99,$55,$55,
                                             $13,$3A,$10,$AA,$66,$66,
                                             $13,$3B,$10,$BB,$77,$77,
                                             $13,$3C,$10,$CC,$44,$44,
                                             $13,$3D,$10,$DD,$55,$55,
                                             $13,$3E,$10,$EE,$66,$66,
                                             $13,$3F,$10,$FF,$77,$77,
                                             $13,$40,$10,$00,$88,$00,
                                             $13,$41,$10,$11,$99,$11,
                                             $13,$42,$10,$22,$AA,$22,
                                             $13,$43,$10,$33,$BB,$33,
                                             $13,$44,$10,$44,$88,$00,
                                             $13,$45,$10,$55,$99,$11,
                                             $13,$46,$10,$66,$AA,$22,
                                             $13,$47,$10,$77,$BB,$33,
                                             $13,$48,$10,$00,$88,$44,
                                             $13,$49,$10,$11,$99,$55,
                                             $13,$4A,$10,$22,$AA,$66,
                                             $13,$4B,$10,$33,$BB,$77,
                                             $13,$4C,$10,$44,$88,$44,
                                             $13,$4D,$10,$55,$99,$55,
                                             $13,$4E,$10,$66,$AA,$66,
                                             $13,$4F,$10,$77,$BB,$77,
                                             $13,$50,$10,$88,$88,$00,
                                             $13,$51,$10,$99,$99,$11,
                                             $13,$52,$10,$AA,$AA,$22,
                                             $13,$53,$10,$BB,$BB,$33,
                                             $13,$54,$10,$CC,$88,$00,
                                             $13,$55,$10,$DD,$99,$11,
                                             $13,$56,$10,$EE,$AA,$22,
                                             $13,$57,$10,$FF,$BB,$33,
                                             $13,$58,$10,$88,$88,$44,
                                             $13,$59,$10,$99,$99,$55,
                                             $13,$5A,$10,$AA,$AA,$66,
                                             $13,$5B,$10,$BB,$BB,$77,
                                             $13,$5C,$10,$CC,$88,$44,
                                             $13,$5D,$10,$DD,$99,$55,
                                             $13,$5E,$10,$EE,$AA,$66,
                                             $13,$5F,$10,$FF,$BB,$77,
                                             $13,$60,$10,$00,$CC,$00,
                                             $13,$61,$10,$11,$DD,$11,
                                             $13,$62,$10,$22,$EE,$22,
                                             $13,$63,$10,$33,$FF,$33,
                                             $13,$64,$10,$44,$CC,$00,
                                             $13,$65,$10,$55,$DD,$11,
                                             $13,$66,$10,$66,$EE,$22,
                                             $13,$67,$10,$77,$FF,$33,
                                             $13,$68,$10,$00,$CC,$44,
                                             $13,$69,$10,$11,$DD,$55,
                                             $13,$6A,$10,$22,$EE,$66,
                                             $13,$6B,$10,$33,$FF,$77,
                                             $13,$6C,$10,$44,$CC,$44,
                                             $13,$6D,$10,$55,$DD,$55,
                                             $13,$6E,$10,$66,$EE,$66,
                                             $13,$6F,$10,$77,$FF,$77,
                                             $13,$70,$10,$88,$CC,$00,
                                             $13,$71,$10,$99,$DD,$11,
                                             $13,$72,$10,$AA,$EE,$22,
                                             $13,$73,$10,$BB,$FF,$33,
                                             $13,$74,$10,$CC,$CC,$00,
                                             $13,$75,$10,$DD,$DD,$11,
                                             $13,$76,$10,$EE,$EE,$22,
                                             $13,$77,$10,$FF,$FF,$33,
                                             $13,$78,$10,$88,$CC,$44,
                                             $13,$79,$10,$99,$DD,$55,
                                             $13,$7A,$10,$AA,$EE,$66,
                                             $13,$7B,$10,$BB,$FF,$77,
                                             $13,$7C,$10,$CC,$CC,$44,
                                             $13,$7D,$10,$DD,$DD,$55,
                                             $13,$7E,$10,$EE,$EE,$66,
                                             $13,$7F,$10,$FF,$FF,$77,
                                             $13,$80,$10,$00,$00,$88,
                                             $13,$81,$10,$11,$11,$99,
                                             $13,$82,$10,$22,$22,$AA,
                                             $13,$83,$10,$33,$33,$BB,
                                             $13,$84,$10,$44,$00,$88,
                                             $13,$85,$10,$55,$11,$99,
                                             $13,$86,$10,$66,$22,$AA,
                                             $13,$87,$10,$77,$33,$BB,
                                             $13,$88,$10,$00,$00,$CC,
                                             $13,$89,$10,$11,$11,$DD,
                                             $13,$8A,$10,$22,$22,$EE,
                                             $13,$8B,$10,$33,$33,$FF,
                                             $13,$8C,$10,$44,$00,$CC,
                                             $13,$8D,$10,$55,$11,$DD,
                                             $13,$8E,$10,$66,$22,$EE,
                                             $13,$8F,$10,$77,$33,$FF,
                                             $13,$90,$10,$88,$00,$88,
                                             $13,$91,$10,$99,$11,$99,
                                             $13,$92,$10,$AA,$22,$AA,
                                             $13,$93,$10,$BB,$33,$BB,
                                             $13,$94,$10,$CC,$00,$88,
                                             $13,$95,$10,$DD,$11,$99,
                                             $13,$96,$10,$EE,$22,$AA,
                                             $13,$97,$10,$FF,$33,$BB,
                                             $13,$98,$10,$88,$00,$CC,
                                             $13,$99,$10,$99,$11,$DD,
                                             $13,$9A,$10,$AA,$22,$EE,
                                             $13,$9B,$10,$BB,$33,$FF,
                                             $13,$9C,$10,$CC,$00,$CC,
                                             $13,$9D,$10,$DD,$11,$DD,
                                             $13,$9E,$10,$EE,$22,$EE,
                                             $13,$9F,$10,$FF,$33,$FF,
                                             $13,$A0,$10,$00,$44,$88,
                                             $13,$A1,$10,$11,$55,$99,
                                             $13,$A2,$10,$22,$66,$AA,
                                             $13,$A3,$10,$33,$77,$BB,
                                             $13,$A4,$10,$44,$44,$88,
                                             $13,$A5,$10,$55,$55,$99,
                                             $13,$A6,$10,$66,$66,$AA,
                                             $13,$A7,$10,$77,$77,$BB,
                                             $13,$A8,$10,$00,$44,$CC,
                                             $13,$A9,$10,$11,$55,$DD,
                                             $13,$AA,$10,$22,$66,$EE,
                                             $13,$AB,$10,$33,$77,$FF,
                                             $13,$AC,$10,$44,$44,$CC,
                                             $13,$AD,$10,$55,$55,$DD,
                                             $13,$AE,$10,$66,$66,$EE,
                                             $13,$AF,$10,$77,$77,$FF,
                                             $13,$B0,$10,$88,$44,$88,
                                             $13,$B1,$10,$99,$55,$99,
                                             $13,$B2,$10,$AA,$66,$AA,
                                             $13,$B3,$10,$BB,$77,$BB,
                                             $13,$B4,$10,$CC,$44,$88,
                                             $13,$B5,$10,$DD,$55,$99,
                                             $13,$B6,$10,$EE,$66,$AA,
                                             $13,$B7,$10,$FF,$77,$BB,
                                             $13,$B8,$10,$88,$44,$CC,
                                             $13,$B9,$10,$99,$55,$DD,
                                             $13,$BA,$10,$AA,$66,$EE,
                                             $13,$BB,$10,$BB,$77,$FF,
                                             $13,$BC,$10,$CC,$44,$CC,
                                             $13,$BD,$10,$DD,$55,$DD,
                                             $13,$BE,$10,$EE,$66,$EE,
                                             $13,$BF,$10,$FF,$77,$FF,
                                             $13,$C0,$10,$00,$88,$88,
                                             $13,$C1,$10,$11,$99,$99,
                                             $13,$C2,$10,$22,$AA,$AA,
                                             $13,$C3,$10,$33,$BB,$BB,
                                             $13,$C4,$10,$44,$88,$88,
                                             $13,$C5,$10,$55,$99,$99,
                                             $13,$C6,$10,$66,$AA,$AA,
                                             $13,$C7,$10,$77,$BB,$BB,
                                             $13,$C8,$10,$00,$88,$CC,
                                             $13,$C9,$10,$11,$99,$DD,
                                             $13,$CA,$10,$22,$AA,$EE,
                                             $13,$CB,$10,$33,$BB,$FF,
                                             $13,$CC,$10,$44,$88,$CC,
                                             $13,$CD,$10,$55,$99,$DD,
                                             $13,$CE,$10,$66,$AA,$EE,
                                             $13,$CF,$10,$77,$BB,$FF,
                                             $13,$D0,$10,$88,$88,$88,
                                             $13,$D1,$10,$99,$99,$99,
                                             $13,$D2,$10,$AA,$AA,$AA,
                                             $13,$D3,$10,$BB,$BB,$BB,
                                             $13,$D4,$10,$CC,$88,$88,
                                             $13,$D5,$10,$DD,$99,$99,
                                             $13,$D6,$10,$EE,$AA,$AA,
                                             $13,$D7,$10,$FF,$BB,$BB,
                                             $13,$D8,$10,$88,$88,$CC,
                                             $13,$D9,$10,$99,$99,$DD,
                                             $13,$DA,$10,$AA,$AA,$EE,
                                             $13,$DB,$10,$BB,$BB,$FF,
                                             $13,$DC,$10,$CC,$88,$CC,
                                             $13,$DD,$10,$DD,$99,$DD,
                                             $13,$DE,$10,$EE,$AA,$EE,
                                             $13,$DF,$10,$FF,$BB,$FF,
                                             $13,$E0,$10,$00,$CC,$88,
                                             $13,$E1,$10,$11,$DD,$99,
                                             $13,$E2,$10,$22,$EE,$AA,
                                             $13,$E3,$10,$33,$FF,$BB,
                                             $13,$E4,$10,$44,$CC,$88,
                                             $13,$E5,$10,$55,$DD,$99,
                                             $13,$E6,$10,$66,$EE,$AA,
                                             $13,$E7,$10,$77,$FF,$BB,
                                             $13,$E8,$10,$00,$CC,$CC,
                                             $13,$E9,$10,$11,$DD,$DD,
                                             $13,$EA,$10,$22,$EE,$EE,
                                             $13,$EB,$10,$33,$FF,$FF,
                                             $13,$EC,$10,$44,$CC,$CC,
                                             $13,$ED,$10,$55,$DD,$DD,
                                             $13,$EE,$10,$66,$EE,$EE,
                                             $13,$EF,$10,$77,$FF,$FF,
                                             $13,$F0,$10,$88,$CC,$88,
                                             $13,$F1,$10,$99,$DD,$99,
                                             $13,$F2,$10,$AA,$EE,$AA,
                                             $13,$F3,$10,$BB,$FF,$BB,
                                             $13,$F4,$10,$CC,$CC,$88,
                                             $13,$F5,$10,$DD,$DD,$99,
                                             $13,$F6,$10,$EE,$EE,$AA,
                                             $13,$F7,$10,$FF,$FF,$BB,
                                             $13,$F8,$10,$88,$CC,$CC,
                                             $13,$F9,$10,$99,$DD,$DD,
                                             $13,$FA,$10,$AA,$EE,$EE,
                                             $13,$FB,$10,$BB,$FF,$FF,
                                             $13,$FC,$10,$CC,$CC,$CC,
                                             $13,$FD,$10,$DD,$DD,$DD,
                                             $13,$FE,$10,$EE,$EE,$EE,
                                             $13,$FF,$10,$FF,$FF,$FF);
