Public Class Form1
    'Public variables:
    Public blackTurn As Boolean = True
    Public positions(7, 7) As String
    Public selected As Boolean
    Public canTake As Boolean = False
    Public over As Boolean = False


    'I misunderstood the rules of draughts, and have set up the pieces wrong. The pieces should all only be on white squares and the board should have a white square in top right and bottom left...

    'Tag rules: (such documentation)
    'tag is a string. Who knows why but that's the way I've set it up for some reason
    'Tag is for cursors, and whether it's the piece from the left or right that has enabled it to be set up using "Left" and "Right"






    Sub CheckTake()
        deselectStuff()
        disableStuff()
        GetPositions()

        Dim piece As PictureBox
        canTake = False

        For row As Integer = 0 To 7
            For col As Integer = 0 To 7
                Dim colour As String = getColour(row, col)

                If colour <> Nothing Then
                    If blackTurn = True Then
                        piece = getBlackPiece(row, col)
                    Else
                        piece = getWhitePiece(row, col)
                    End If

                    If blackTurn = True Then
                        If row >= 2 Then
                            If col >= 2 Then
                                Dim upLeftColour As String = getColour(row - 1, col - 1)
                                If upLeftColour = "White" Then
                                    Dim upLeft2Colour As String = getColour(row - 2, col - 2)
                                    If upLeft2Colour = Nothing Then
                                        canTake = True
                                        Dim upLeft2Cursor As PictureBox = getCursor(row - 2, col - 2)
                                        upLeft2Cursor.Visible = True
                                        upLeft2Cursor.Enabled = True
                                        If upLeft2Cursor.Tag <> "TakeRight" Then
                                            upLeft2Cursor.Tag = "TakeLeft"
                                        Else
                                            upLeft2Cursor.Tag = "TakeBoth"
                                        End If
                                    End If
                                End If
                            End If


                            If col <= 5 Then
                                Dim upRightColour As String = getColour(row - 1, col + 1)
                                If upRightColour = "White" Then
                                    Dim upRight2Colour As String = getColour(row - 2, col + 2)
                                    If upRight2Colour = Nothing Then
                                        canTake = True
                                        Dim upRight2Cursor As PictureBox = getCursor(row - 2, col + 2)
                                        upRight2Cursor.Visible = True
                                        upRight2Cursor.Enabled = True
                                        If upRight2Cursor.Tag <> "TakeLeft" Then
                                            upRight2Cursor.Tag = "TakeRight"
                                        Else
                                            upRight2Cursor.Tag = "TakeBoth"
                                        End If
                                    End If
                                End If
                            End If
                        End If


                    Else
                        If row <= 5 Then
                            If col >= 2 Then
                                Dim downLeftColour As String = getColour(row + 1, col - 1)
                                If downLeftColour = "Black" Then
                                    Dim downLeft2Colour As String = getColour(row + 2, col - 2)
                                    If downLeft2Colour = Nothing Then
                                        canTake = True
                                        Dim downLeft2Cursor As PictureBox = getCursor(row + 2, col - 2)
                                        downLeft2Cursor.Visible = True
                                        downLeft2Cursor.Enabled = True
                                        If downLeft2Cursor.Tag <> "TakeRight" Then
                                            downLeft2Cursor.Tag = "TakeLeft"
                                        Else
                                            downLeft2Cursor.Tag = "TakeBoth"
                                        End If
                                    End If
                                End If
                            End If


                            If col <= 5 Then
                                Dim downRightColour As String = getColour(row + 1, col + 1)
                                If downRightColour = "Black" Then
                                    Dim downRight2Colour As String = getColour(row + 2, col + 2)
                                    If downRight2Colour = Nothing Then
                                        canTake = True
                                        Dim downRight2Cursor As PictureBox = getCursor(row + 2, col + 2)
                                        downRight2Cursor.Visible = True
                                        downRight2Cursor.Enabled = True
                                        If downRight2Cursor.Tag <> "TakeLeft" Then
                                            downRight2Cursor.Tag = "TakeRight"
                                        Else
                                            downRight2Cursor.Tag = "TakeBoth"
                                        End If
                                    End If
                                End If
                            End If
                        End If




                    End If





                End If
            Next
        Next


        If canTake = False Then
            deselectStuff()
            enableStuff()
        End If
    End Sub



    Sub moveToCursor(ref)

        GetPositions()

        disableStuff()



        Dim index() As Integer = getIndex(ref)
        Dim row As Integer = index(0)
        Dim col As Integer = index(1)
        Dim cursor As PictureBox = getCursor(row, col)
        Dim tag As String = cursor.Tag
        Dim piece As PictureBox = Nothing
        Dim newPiece As PictureBox


        If blackTurn = True Then
            If tag = "Left" Then
                piece = getBlackPiece(row + 1, col - 1)

            ElseIf tag = "Right" Then
                piece = getBlackPiece(row + 1, col + 1)


            Else
                Dim deletedPiece As PictureBox = Nothing
                If tag = "TakeRight" Then
                    piece = getBlackPiece(row + 2, col - 2)
                    deletedPiece = getWhitePiece(row + 1, col - 1)



                ElseIf tag = "TakeLeft" Then
                    piece = getBlackPiece(row + 2, col + 2)
                    deletedPiece = getWhitePiece(row + 1, col + 1)



                ElseIf tag = "TakeBoth" Then
                    Dim deletedPiece2 As PictureBox
                    deletedPiece = getWhitePiece(row + 1, col - 1)
                    deletedPiece2 = getWhitePiece(row + 1, col + 1)

                    deletedPiece2.Visible = False

                End If


                deletedPiece.Visible = False
            End If




            piece.Visible = False

            newPiece = getBlackPiece(row, col)
            newPiece.Visible = True


        Else
            If tag = "Left" Then
                piece = getWhitePiece(row - 1, col - 1)

            ElseIf tag = "Right" Then
                piece = getWhitePiece(row - 1, col + 1)




            Else
                Dim deletedPiece As PictureBox = Nothing
                If tag = "TakeRight" Then
                    piece = getWhitePiece(row - 2, col - 2)
                    deletedPiece = getBlackPiece(row - 1, col - 1)



                ElseIf tag = "TakeLeft" Then
                    piece = getWhitePiece(row - 2, col + 2)
                    deletedPiece = getBlackPiece(row - 1, col + 1)



                ElseIf tag = "TakeBoth" Then
                    Dim piece2 As PictureBox
                    Dim deletedPiece2 As PictureBox
                    piece = getWhitePiece(row - 2, col - 2)
                    piece2 = getWhitePiece(row + 2, col + 2)
                    deletedPiece = getWhitePiece(row + 1, col - 1)
                    deletedPiece2 = getWhitePiece(row + 1, col + 1)

                    deletedPiece2.Visible = False

                End If


                deletedPiece.Visible = False
            End If





            piece.Visible = False

            newPiece = getWhitePiece(row, col)
            newPiece.Visible = True
        End If



        blackTurn = Not blackTurn


        GetPositions()
        disableStuff()
        deselectStuff()
        enableStuff()
        CheckTake()

    End Sub




    Sub deselectStuff()
        Dim cursor As PictureBox

        For row = 0 To 7
            For col = 0 To 7
                If row Mod 2 = 0 Then
                    If col Mod 2 = 1 Then
                        cursor = getCursor(row, col)
                        cursor.Enabled = False
                        cursor.Visible = False
                        cursor.Tag = Nothing
                    End If
                Else
                    If col Mod 2 = 0 Then
                        cursor = getCursor(row, col)
                        cursor.Enabled = False
                        cursor.Visible = False
                        cursor.Tag = Nothing
                    End If
                End If
            Next
        Next
    End Sub


    Sub enableStuff()
        GetPositions()


        Dim piece As PictureBox

        For row As Integer = 0 To 7
            For col As Integer = 0 To 7
                If getColour(row, col) <> Nothing Then
                    If blackTurn = True Then
                        If getColour(row, col) = "Black" Then
                            If row >= 1 Then
                                piece = getBlackPiece(row, col)
                                piece.Enabled = True
                            End If
                        End If
                    Else
                        If getColour(row, col) = "White" Then
                            If row <= 6 Then
                                piece = getWhitePiece(row, col)
                                piece.Enabled = True
                            End If
                        End If
                    End If
                End If
            Next
        Next
    End Sub

    Sub disableStuff()
        GetPositions()
        Dim piece As PictureBox

        For row As Integer = 0 To 7
            For col As Integer = 0 To 7
                If getColour(row, col) <> Nothing Then
                    If blackTurn = True Then
                        piece = getBlackPiece(row, col)
                        piece.Enabled = False
                    ElseIf blackTurn = False Then
                        piece = getWhitePiece(row, col)
                        piece.Enabled = False
                    End If
                End If
            Next
        Next
    End Sub


    Sub prepMove(row, col)

        GetPositions()

        Dim piece As PictureBox = Nothing
        Dim pieceExists As Boolean = True
        Dim colour As String
        colour = getColour(row, col)
        If colour <> Nothing Then
            If blackTurn = True Then
                piece = getBlackPiece(row, col)
            ElseIf blackTurn = False Then
                piece = getWhitePiece(row, col)
            End If


            If pieceExists = True AndAlso piece.Visible = True Then
                If blackTurn = True Then
                    If row >= 1 Then
                        Dim checkUpLeft(1) As Integer
                        Dim checkUpRight(1) As Integer
                        Dim upLeftColour As String
                        Dim upRightColour As String
                        If col >= 1 Then
                            checkUpLeft(0) = row - 1
                            checkUpLeft(1) = col - 1
                            upLeftColour = getColour(checkUpLeft(0), checkUpLeft(1))

                            If upLeftColour = Nothing Then
                                Dim cursor As PictureBox = getCursor(checkUpLeft(0), checkUpLeft(1))
                                cursor.Visible = True
                                cursor.Enabled = True
                                If cursor.Tag <> "Left" AndAlso cursor.Tag <> "Both" Then
                                    cursor.Tag = "Right"
                                Else
                                    cursor.Tag = "Both"
                                End If
                            End If


                        End If
                        If col <= 6 Then
                            checkUpRight(0) = row - 1
                            checkUpRight(1) = col + 1
                            upRightColour = getColour(checkUpRight(0), checkUpRight(1))

                            If upRightColour = Nothing Then
                                Dim cursor As PictureBox = getCursor(checkUpRight(0), checkUpRight(1))
                                cursor.Visible = True
                                cursor.Enabled = True
                                If cursor.Tag <> "Right" AndAlso cursor.Tag <> "Both" Then
                                    cursor.Tag = "Left"
                                Else
                                    cursor.Tag = "Both"
                                End If
                            End If
                        End If
                    End If
                ElseIf blackTurn = False Then
                    If row <= 6 Then
                        Dim checkDownLeft(1) As Integer
                        Dim checkDownRight(1) As Integer
                        Dim downLeftColour As String
                        Dim downRightColour As String
                        If col >= 1 Then
                            checkDownLeft(0) = row + 1
                            checkDownLeft(1) = col - 1
                            downLeftColour = getColour(checkDownLeft(0), checkDownLeft(1))

                            If downLeftColour = Nothing Then
                                Dim cursor As PictureBox = getCursor(checkDownLeft(0), checkDownLeft(1))
                                cursor.Visible = True
                                cursor.Enabled = True
                                If cursor.Tag <> "Left" AndAlso cursor.Tag <> "Both" Then
                                    cursor.Tag = "Right"
                                Else
                                    cursor.Tag = "Both"
                                End If
                            End If


                        End If
                        If col <= 6 Then
                            checkDownRight(0) = row + 1
                            checkDownRight(1) = col + 1
                            downRightColour = getColour(checkDownRight(0), checkDownRight(1))

                            If downRightColour = Nothing Then
                                Dim cursor As PictureBox = getCursor(checkDownRight(0), checkDownRight(1))
                                cursor.Visible = True
                                cursor.Enabled = True
                                If cursor.Tag <> "Right" AndAlso cursor.Tag <> "Both" Then
                                    cursor.Tag = "Left"
                                Else
                                    cursor.Tag = "Both"
                                End If
                            End If
                        End If
                    End If
                End If


            End If
        End If



    End Sub

    Function getColour(row, col)
        Dim colour As String = Nothing
        Dim whitePiece As PictureBox
        Dim blackPiece As PictureBox
        Dim whiteTrue As Boolean = False

        If row Mod 2 = 1 Then
            If col Mod 2 = 0 Then
                whitePiece = getWhitePiece(row, col)
                blackPiece = getBlackPiece(row, col)

                If whitePiece.Visible = True Then
                    colour = "White"
                    whiteTrue = True
                ElseIf whiteTrue = False AndAlso blackPiece.Visible = True Then
                    colour = "Black"
                Else
                    colour = Nothing
                End If
            End If
        Else
            If col Mod 2 = 1 Then
                whitePiece = getWhitePiece(row, col)
                blackPiece = getBlackPiece(row, col)

                If whitePiece.Visible = True Then
                    colour = "White"
                    whiteTrue = True
                ElseIf whiteTrue = False AndAlso blackPiece.Visible = True Then
                    colour = "Black"
                Else
                    colour = Nothing
                End If
            End If
        End If


        Return colour
    End Function


    Function getBlackPiece(row, col)
        Dim ref As Integer = getRef(row, col)
        Dim piece As PictureBox = Nothing
        Dim colour As String = "Black"

        piece = CType(Me.Controls(colour & ref), PictureBox)
        Return piece
    End Function

    Function getCursor(row, col)
        Dim ref As Integer = getRef(row, col)
        Dim piece As PictureBox = Nothing
        Dim colour As String = "Cursor"

        piece = CType(Me.Controls(colour & ref), PictureBox)
        Return piece
    End Function

    Function getWhitePiece(row, col)
        Dim ref As Integer = getRef(row, col)
        Dim piece As PictureBox = Nothing
        Dim colour As String = "White"

        piece = CType(Me.Controls(colour & ref), PictureBox)
        Return piece
    End Function


    Function getRef(row, col)
        Dim ref As Integer
        Dim rowVal As Integer = row * 4
        Dim colVal As Integer
        Dim exists As Boolean = False
        If row Mod 2 = 1 Then
            If col Mod 2 = 0 Then
                colVal = col \ 2
                exists = True
            Else
                colVal = Nothing
            End If
        Else
            If col Mod 2 = 1 Then
                colVal = col \ 2
                exists = True
            Else
                colVal = Nothing
            End If
        End If

        If exists = True Then
            ref = rowVal + colVal + 1
        End If
        Return ref
    End Function

    Function getIndex(ref)
        ref -= 1
        Dim row As Integer = ref \ 4
        Dim col As Integer

        If row Mod 2 = 0 Then
            col = ((ref Mod 4) * 2) + 1
        Else
            col = (ref Mod 4) * 2
        End If

        Dim index(1) As Integer
        index(0) = row
        index(1) = col

        Return index
    End Function


    Sub GetPositions()
        Dim colour As String

        For row As Integer = 0 To 7
            For col As Integer = 0 To 7
                colour = getColour(row, col)

                If colour = "Black" Then
                    positions(row, col) = "Black"
                ElseIf colour = "White" Then
                    positions(row, col) = "White"
                Else
                    positions(row, col) = Nothing

                End If

            Next
        Next
    End Sub


    Sub Initialise()
        Dim piece As PictureBox
        Dim positions(7, 7) As String

        For row As Integer = 0 To 7
            For col As Integer = 0 To 7
                If row <= 2 Then
                    If row Mod 2 = 1 Then
                        If col Mod 2 = 0 Then
                            piece = getWhitePiece(row, col)
                            piece.Visible = True
                            positions(row, col) = "White"
                        End If
                    Else
                        If col Mod 2 = 1 Then
                            piece = getWhitePiece(row, col)
                            piece.Visible = True
                            positions(row, col) = "White"
                        End If
                    End If
                ElseIf row >= 5 Then
                    If row Mod 2 = 1 Then
                        If col Mod 2 = 0 Then
                            piece = getBlackPiece(row, col)
                            piece.Visible = True
                            positions(row, col) = "Black"
                        End If
                    Else
                        If col Mod 2 = 1 Then
                            piece = getBlackPiece(row, col)
                            piece.Visible = True
                            positions(row, col) = "Black"
                        End If
                    End If
                End If
            Next
        Next


        For row As Integer = 0 To 7
            For col As Integer = 0 To 7
                Dim colour As String = getColour(row, col)
                Dim data(10) As String
                If colour = "White" Then
                    piece = getWhitePiece(row, col)
                    piece.Tag = data

                ElseIf colour = "Black" Then
                    piece = getBlackPiece(row, col)
                    piece.Tag = data

                End If

                If colour <> Nothing Then
                    piece = getCursor(row, col)
                    piece.Tag = data
                End If
            Next
        Next

        GetPositions()
        enableStuff()
    End Sub



    Private Sub Form1_Load(sender As Object, e As EventArgs) Handles MyBase.Load

        Initialise()

    End Sub






















    'Click handling, this will be long so don't read it, it's not worth it
    Private Sub Black1_Click(sender As Object, e As EventArgs) Handles Black1.Click
        deselectStuff()
        Dim index() As Integer = getIndex(1)
        Dim row As Integer = index(0)
        Dim col As Integer = index(1)
        prepMove(row, col)
    End Sub
    Private Sub Black2_Click(sender As Object, e As EventArgs) Handles Black2.Click
        deselectStuff()
        Dim index() As Integer = getIndex(2)
        Dim row As Integer = index(0)
        Dim col As Integer = index(1)
        prepMove(row, col)
    End Sub
    Private Sub Black3_Click(sender As Object, e As EventArgs) Handles Black3.Click
        deselectStuff()
        Dim index() As Integer = getIndex(3)
        Dim row As Integer = index(0)
        Dim col As Integer = index(1)
        prepMove(row, col)
    End Sub
    Private Sub Black4_Click(sender As Object, e As EventArgs) Handles Black4.Click
        deselectStuff()
        Dim index() As Integer = getIndex(4)
        Dim row As Integer = index(0)
        Dim col As Integer = index(1)
        prepMove(row, col)
    End Sub
    Private Sub Black5_Click(sender As Object, e As EventArgs) Handles Black5.Click
        deselectStuff()
        Dim index() As Integer = getIndex(5)
        Dim row As Integer = index(0)
        Dim col As Integer = index(1)
        prepMove(row, col)
    End Sub
    Private Sub Black6_Click(sender As Object, e As EventArgs) Handles Black6.Click
        deselectStuff()
        Dim index() As Integer = getIndex(6)
        Dim row As Integer = index(0)
        Dim col As Integer = index(1)
        prepMove(row, col)
    End Sub
    Private Sub Black7_Click(sender As Object, e As EventArgs) Handles Black7.Click
        deselectStuff()
        Dim index() As Integer = getIndex(7)
        Dim row As Integer = index(0)
        Dim col As Integer = index(1)
        prepMove(row, col)
    End Sub
    Private Sub Black8_Click(sender As Object, e As EventArgs) Handles Black8.Click
        deselectStuff()
        Dim index() As Integer = getIndex(8)
        Dim row As Integer = index(0)
        Dim col As Integer = index(1)
        prepMove(row, col)
    End Sub
    Private Sub Black9_Click(sender As Object, e As EventArgs) Handles Black9.Click
        deselectStuff()
        Dim index() As Integer = getIndex(9)
        Dim row As Integer = index(0)
        Dim col As Integer = index(1)
        prepMove(row, col)
    End Sub
    Private Sub Black10_Click(sender As Object, e As EventArgs) Handles Black10.Click
        deselectStuff()
        Dim index() As Integer = getIndex(10)
        Dim row As Integer = index(0)
        Dim col As Integer = index(1)
        prepMove(row, col)
    End Sub
    Private Sub Black11_Click(sender As Object, e As EventArgs) Handles Black11.Click
        deselectStuff()
        Dim index() As Integer = getIndex(11)
        Dim row As Integer = index(0)
        Dim col As Integer = index(1)
        prepMove(row, col)
    End Sub
    Private Sub Black12_Click(sender As Object, e As EventArgs) Handles Black12.Click
        deselectStuff()
        Dim index() As Integer = getIndex(12)
        Dim row As Integer = index(0)
        Dim col As Integer = index(1)
        prepMove(row, col)
    End Sub
    Private Sub Black13_Click(sender As Object, e As EventArgs) Handles Black13.Click
        deselectStuff()
        Dim index() As Integer = getIndex(13)
        Dim row As Integer = index(0)
        Dim col As Integer = index(1)
        prepMove(row, col)
    End Sub
    Private Sub Black14_Click(sender As Object, e As EventArgs) Handles Black14.Click
        deselectStuff()
        Dim index() As Integer = getIndex(14)
        Dim row As Integer = index(0)
        Dim col As Integer = index(1)
        prepMove(row, col)
    End Sub
    Private Sub Black15_Click(sender As Object, e As EventArgs) Handles Black15.Click
        deselectStuff()
        Dim index() As Integer = getIndex(15)
        Dim row As Integer = index(0)
        Dim col As Integer = index(1)
        prepMove(row, col)
    End Sub
    Private Sub Black16_Click(sender As Object, e As EventArgs) Handles Black16.Click
        deselectStuff()
        Dim index() As Integer = getIndex(16)
        Dim row As Integer = index(0)
        Dim col As Integer = index(1)
        prepMove(row, col)
    End Sub
    Private Sub Black17_Click(sender As Object, e As EventArgs) Handles Black17.Click
        deselectStuff()
        Dim index() As Integer = getIndex(17)
        Dim row As Integer = index(0)
        Dim col As Integer = index(1)
        prepMove(row, col)
    End Sub
    Private Sub Black18_Click(sender As Object, e As EventArgs) Handles Black18.Click
        deselectStuff()
        Dim index() As Integer = getIndex(18)
        Dim row As Integer = index(0)
        Dim col As Integer = index(1)
        prepMove(row, col)
    End Sub
    Private Sub Black19_Click(sender As Object, e As EventArgs) Handles Black19.Click
        deselectStuff()
        Dim index() As Integer = getIndex(19)
        Dim row As Integer = index(0)
        Dim col As Integer = index(1)
        prepMove(row, col)
    End Sub
    Private Sub Black20_Click(sender As Object, e As EventArgs) Handles Black20.Click
        deselectStuff()
        Dim index() As Integer = getIndex(20)
        Dim row As Integer = index(0)
        Dim col As Integer = index(1)
        prepMove(row, col)
    End Sub
    Private Sub Black21_Click(sender As Object, e As EventArgs) Handles Black21.Click
        deselectStuff()
        Dim index() As Integer = getIndex(21)
        Dim row As Integer = index(0)
        Dim col As Integer = index(1)
        prepMove(row, col)
    End Sub
    Private Sub Black22_Click(sender As Object, e As EventArgs) Handles Black22.Click
        deselectStuff()
        Dim index() As Integer = getIndex(22)
        Dim row As Integer = index(0)
        Dim col As Integer = index(1)
        prepMove(row, col)
    End Sub
    Private Sub Black23_Click(sender As Object, e As EventArgs) Handles Black23.Click
        deselectStuff()
        Dim index() As Integer = getIndex(23)
        Dim row As Integer = index(0)
        Dim col As Integer = index(1)
        prepMove(row, col)
    End Sub
    Private Sub Black24_Click(sender As Object, e As EventArgs) Handles Black24.Click
        deselectStuff()
        Dim index() As Integer = getIndex(24)
        Dim row As Integer = index(0)
        Dim col As Integer = index(1)
        prepMove(row, col)
    End Sub


    Private Sub Black25_Click(sender As Object, e As EventArgs) Handles Black25.Click
        deselectStuff()
        Dim index() As Integer = getIndex(25)
        Dim row As Integer = index(0)
        Dim col As Integer = index(1)
        prepMove(row, col)
    End Sub
    Private Sub Black26_Click(sender As Object, e As EventArgs) Handles Black26.Click
        deselectStuff()
        Dim index() As Integer = getIndex(26)
        Dim row As Integer = index(0)
        Dim col As Integer = index(1)
        prepMove(row, col)
    End Sub
    Private Sub Black27_Click(sender As Object, e As EventArgs) Handles Black27.Click
        deselectStuff()
        Dim index() As Integer = getIndex(27)
        Dim row As Integer = index(0)
        Dim col As Integer = index(1)
        prepMove(row, col)
    End Sub
    Private Sub Black28_Click(sender As Object, e As EventArgs) Handles Black28.Click
        deselectStuff()
        Dim index() As Integer = getIndex(28)
        Dim row As Integer = index(0)
        Dim col As Integer = index(1)
        prepMove(row, col)
    End Sub
    Private Sub Black29_Click(sender As Object, e As EventArgs) Handles Black29.Click
        deselectStuff()
        Dim index() As Integer = getIndex(29)
        Dim row As Integer = index(0)
        Dim col As Integer = index(1)
        prepMove(row, col)
    End Sub
    Private Sub Black30_Click(sender As Object, e As EventArgs) Handles Black30.Click
        deselectStuff()
        Dim index() As Integer = getIndex(30)
        Dim row As Integer = index(0)
        Dim col As Integer = index(1)
        prepMove(row, col)
    End Sub
    Private Sub Black31_Click(sender As Object, e As EventArgs) Handles Black31.Click
        deselectStuff()
        Dim index() As Integer = getIndex(31)
        Dim row As Integer = index(0)
        Dim col As Integer = index(1)
        prepMove(row, col)
    End Sub
    Private Sub Black32_Click(sender As Object, e As EventArgs) Handles Black32.Click
        deselectStuff()
        Dim index() As Integer = getIndex(32)
        Dim row As Integer = index(0)
        Dim col As Integer = index(1)
        prepMove(row, col)
    End Sub





    'White buttons - not being clicked rn
    Private Sub White1_Click(sender As Object, e As EventArgs) Handles White1.Click
        deselectStuff()
        Dim index() As Integer = getIndex(1)
        Dim row As Integer = index(0)
        Dim col As Integer = index(1)
        prepMove(row, col)
    End Sub
    Private Sub White2_Click(sender As Object, e As EventArgs) Handles White2.Click
        deselectStuff()
        Dim index() As Integer = getIndex(2)
        Dim row As Integer = index(0)
        Dim col As Integer = index(1)
        prepMove(row, col)
    End Sub
    Private Sub White3_Click(sender As Object, e As EventArgs) Handles White3.Click
        deselectStuff()
        Dim index() As Integer = getIndex(3)
        Dim row As Integer = index(0)
        Dim col As Integer = index(1)
        prepMove(row, col)
    End Sub
    Private Sub White4_Click(sender As Object, e As EventArgs) Handles White4.Click
        deselectStuff()
        Dim index() As Integer = getIndex(4)
        Dim row As Integer = index(0)
        Dim col As Integer = index(1)
        prepMove(row, col)
    End Sub
    Private Sub White5_Click(sender As Object, e As EventArgs) Handles White5.Click
        deselectStuff()
        Dim index() As Integer = getIndex(5)
        Dim row As Integer = index(0)
        Dim col As Integer = index(1)
        prepMove(row, col)
    End Sub
    Private Sub White6_Click(sender As Object, e As EventArgs) Handles White6.Click
        deselectStuff()
        Dim index() As Integer = getIndex(6)
        Dim row As Integer = index(0)
        Dim col As Integer = index(1)
        prepMove(row, col)
    End Sub
    Private Sub White7_Click(sender As Object, e As EventArgs) Handles White7.Click
        deselectStuff()
        Dim index() As Integer = getIndex(7)
        Dim row As Integer = index(0)
        Dim col As Integer = index(1)
        prepMove(row, col)
    End Sub
    Private Sub White8_Click(sender As Object, e As EventArgs) Handles White8.Click
        deselectStuff()
        Dim index() As Integer = getIndex(8)
        Dim row As Integer = index(0)
        Dim col As Integer = index(1)
        prepMove(row, col)
    End Sub
    Private Sub White9_Click(sender As Object, e As EventArgs) Handles White9.Click
        deselectStuff()
        Dim index() As Integer = getIndex(9)
        Dim row As Integer = index(0)
        Dim col As Integer = index(1)
        prepMove(row, col)
    End Sub

    Private Sub White10_Click(sender As Object, e As EventArgs) Handles White10.Click
        deselectStuff()
        Dim index() As Integer = getIndex(10)
        Dim row As Integer = index(0)
        Dim col As Integer = index(1)
        prepMove(row, col)
    End Sub
    Private Sub White11_Click(sender As Object, e As EventArgs) Handles White11.Click
        deselectStuff()
        Dim index() As Integer = getIndex(11)
        Dim row As Integer = index(0)
        Dim col As Integer = index(1)
        prepMove(row, col)
    End Sub
    Private Sub White12_Click(sender As Object, e As EventArgs) Handles White12.Click
        deselectStuff()
        Dim index() As Integer = getIndex(12)
        Dim row As Integer = index(0)
        Dim col As Integer = index(1)
        prepMove(row, col)
    End Sub
    Private Sub White13_Click(sender As Object, e As EventArgs) Handles White13.Click
        deselectStuff()
        Dim index() As Integer = getIndex(13)
        Dim row As Integer = index(0)
        Dim col As Integer = index(1)
        prepMove(row, col)
    End Sub
    Private Sub White14_Click(sender As Object, e As EventArgs) Handles White14.Click
        deselectStuff()
        Dim index() As Integer = getIndex(14)
        Dim row As Integer = index(0)
        Dim col As Integer = index(1)
        prepMove(row, col)
    End Sub
    Private Sub White15_Click(sender As Object, e As EventArgs) Handles White15.Click
        deselectStuff()
        Dim index() As Integer = getIndex(15)
        Dim row As Integer = index(0)
        Dim col As Integer = index(1)
        prepMove(row, col)
    End Sub
    Private Sub White16_Click(sender As Object, e As EventArgs) Handles White16.Click
        deselectStuff()
        Dim index() As Integer = getIndex(16)
        Dim row As Integer = index(0)
        Dim col As Integer = index(1)
        prepMove(row, col)
    End Sub
    Private Sub White17_Click(sender As Object, e As EventArgs) Handles White17.Click
        deselectStuff()
        Dim index() As Integer = getIndex(17)
        Dim row As Integer = index(0)
        Dim col As Integer = index(1)
        prepMove(row, col)
    End Sub
    Private Sub White18_Click(sender As Object, e As EventArgs) Handles White18.Click
        deselectStuff()
        Dim index() As Integer = getIndex(18)
        Dim row As Integer = index(0)
        Dim col As Integer = index(1)
        prepMove(row, col)
    End Sub
    Private Sub White19_Click(sender As Object, e As EventArgs) Handles White19.Click
        deselectStuff()
        Dim index() As Integer = getIndex(19)
        Dim row As Integer = index(0)
        Dim col As Integer = index(1)
        prepMove(row, col)
    End Sub
    Private Sub White20_Click(sender As Object, e As EventArgs) Handles White20.Click
        deselectStuff()
        Dim index() As Integer = getIndex(20)
        Dim row As Integer = index(0)
        Dim col As Integer = index(1)
        prepMove(row, col)
    End Sub
    Private Sub White21_Click(sender As Object, e As EventArgs) Handles White21.Click
        deselectStuff()
        Dim index() As Integer = getIndex(21)
        Dim row As Integer = index(0)
        Dim col As Integer = index(1)
        prepMove(row, col)
    End Sub
    Private Sub White22_Click(sender As Object, e As EventArgs) Handles White22.Click
        deselectStuff()
        Dim index() As Integer = getIndex(22)
        Dim row As Integer = index(0)
        Dim col As Integer = index(1)
        prepMove(row, col)
    End Sub
    Private Sub White23_Click(sender As Object, e As EventArgs) Handles White23.Click
        deselectStuff()
        Dim index() As Integer = getIndex(23)
        Dim row As Integer = index(0)
        Dim col As Integer = index(1)
        prepMove(row, col)
    End Sub
    Private Sub White24_Click(sender As Object, e As EventArgs) Handles White24.Click
        deselectStuff()
        Dim index() As Integer = getIndex(24)
        Dim row As Integer = index(0)
        Dim col As Integer = index(1)
        prepMove(row, col)
    End Sub
    Private Sub White25_Click(sender As Object, e As EventArgs) Handles White25.Click
        deselectStuff()
        Dim index() As Integer = getIndex(25)
        Dim row As Integer = index(0)
        Dim col As Integer = index(1)
        prepMove(row, col)
    End Sub
    Private Sub White26_Click(sender As Object, e As EventArgs) Handles White26.Click
        deselectStuff()
        Dim index() As Integer = getIndex(26)
        Dim row As Integer = index(0)
        Dim col As Integer = index(1)
        prepMove(row, col)
    End Sub
    Private Sub White27_Click(sender As Object, e As EventArgs) Handles White27.Click
        deselectStuff()
        Dim index() As Integer = getIndex(27)
        Dim row As Integer = index(0)
        Dim col As Integer = index(1)
        prepMove(row, col)
    End Sub
    Private Sub White28_Click(sender As Object, e As EventArgs) Handles White28.Click
        deselectStuff()
        Dim index() As Integer = getIndex(28)
        Dim row As Integer = index(0)
        Dim col As Integer = index(1)
        prepMove(row, col)
    End Sub
    Private Sub White29_Click(sender As Object, e As EventArgs) Handles White29.Click
        deselectStuff()
        Dim index() As Integer = getIndex(29)
        Dim row As Integer = index(0)
        Dim col As Integer = index(1)
        prepMove(row, col)
    End Sub
    Private Sub White30_Click(sender As Object, e As EventArgs) Handles White30.Click
        deselectStuff()
        Dim index() As Integer = getIndex(30)
        Dim row As Integer = index(0)
        Dim col As Integer = index(1)
        prepMove(row, col)
    End Sub
    Private Sub White31_Click(sender As Object, e As EventArgs) Handles White31.Click
        deselectStuff()
        Dim index() As Integer = getIndex(31)
        Dim row As Integer = index(0)
        Dim col As Integer = index(1)
        prepMove(row, col)
    End Sub
    Private Sub White32_Click(sender As Object, e As EventArgs) Handles White32.Click
        deselectStuff()
        Dim index() As Integer = getIndex(32)
        Dim row As Integer = index(0)
        Dim col As Integer = index(1)
        prepMove(row, col)
    End Sub









    'Cusror handling
    Private Sub Cursor1_Click(sender As Object, e As EventArgs) Handles Cursor1.Click
        moveToCursor(1)
    End Sub
    Private Sub Cursor2_Click(sender As Object, e As EventArgs) Handles Cursor2.Click
        moveToCursor(2)
    End Sub
    Private Sub Cursor3_Click(sender As Object, e As EventArgs) Handles Cursor3.Click
        moveToCursor(3)
    End Sub
    Private Sub Cursor4_Click(sender As Object, e As EventArgs) Handles Cursor4.Click
        moveToCursor(4)
    End Sub
    Private Sub Cursor5_Click(sender As Object, e As EventArgs) Handles Cursor5.Click
        moveToCursor(5)
    End Sub
    Private Sub Cursor6_Click(sender As Object, e As EventArgs) Handles Cursor6.Click
        moveToCursor(6)
    End Sub
    Private Sub Cursor7_Click(sender As Object, e As EventArgs) Handles Cursor7.Click
        moveToCursor(7)
    End Sub
    Private Sub Cursor8_Click(sender As Object, e As EventArgs) Handles Cursor8.Click
        moveToCursor(8)
    End Sub
    Private Sub Cursor9_Click(sender As Object, e As EventArgs) Handles Cursor9.Click
        moveToCursor(9)
    End Sub
    Private Sub Cursor10_Click(sender As Object, e As EventArgs) Handles Cursor10.Click
        moveToCursor(10)
    End Sub
    Private Sub Cursor11_Click(sender As Object, e As EventArgs) Handles Cursor11.Click
        moveToCursor(11)
    End Sub
    Private Sub Cursor12_Click(sender As Object, e As EventArgs) Handles Cursor12.Click
        moveToCursor(12)
    End Sub
    Private Sub Cursor13_Click(sender As Object, e As EventArgs) Handles Cursor13.Click
        moveToCursor(13)
    End Sub
    Private Sub Cursor14_Click(sender As Object, e As EventArgs) Handles Cursor14.Click
        moveToCursor(14)
    End Sub
    Private Sub Cursor15_Click(sender As Object, e As EventArgs) Handles Cursor15.Click
        moveToCursor(15)
    End Sub
    Private Sub Cursor16_Click(sender As Object, e As EventArgs) Handles Cursor16.Click
        moveToCursor(16)
    End Sub
    Private Sub Cursor17_Click(sender As Object, e As EventArgs) Handles Cursor17.Click
        moveToCursor(17)
    End Sub



    Private Sub Cursor18_Click(sender As Object, e As EventArgs) Handles Cursor18.Click
        moveToCursor(18)
    End Sub
    Private Sub Cursor19_Click(sender As Object, e As EventArgs) Handles Cursor19.Click
        moveToCursor(19)
    End Sub
    Private Sub Cursor20_Click(sender As Object, e As EventArgs) Handles Cursor20.Click
        moveToCursor(20)
    End Sub
    Private Sub Cursor21_Click(sender As Object, e As EventArgs) Handles Cursor21.Click
        moveToCursor(21)
    End Sub
    Private Sub Cursor22_Click(sender As Object, e As EventArgs) Handles Cursor22.Click
        moveToCursor(22)
    End Sub
    Private Sub Cursor23_Click(sender As Object, e As EventArgs) Handles Cursor23.Click
        moveToCursor(23)
    End Sub
    Private Sub Cursor24_Click(sender As Object, e As EventArgs) Handles Cursor24.Click
        moveToCursor(24)
    End Sub
    Private Sub Cursor25_Click(sender As Object, e As EventArgs) Handles Cursor25.Click
        moveToCursor(25)
    End Sub
    Private Sub Cursor26_Click(sender As Object, e As EventArgs) Handles Cursor26.Click
        moveToCursor(26)
    End Sub
    Private Sub Cursor27_Click(sender As Object, e As EventArgs) Handles Cursor27.Click
        moveToCursor(27)
    End Sub
    Private Sub Cursor28_Click(sender As Object, e As EventArgs) Handles Cursor28.Click
        moveToCursor(28)
    End Sub
    Private Sub Cursor29_Click(sender As Object, e As EventArgs) Handles Cursor29.Click
        moveToCursor(29)
    End Sub
    Private Sub Cursor30_Click(sender As Object, e As EventArgs) Handles Cursor30.Click
        moveToCursor(30)
    End Sub
    Private Sub Cursor31_Click(sender As Object, e As EventArgs) Handles Cursor31.Click
        moveToCursor(31)
    End Sub
    Private Sub Cursor32_Click(sender As Object, e As EventArgs) Handles Cursor32.Click
        moveToCursor(32)
    End Sub
End Class
