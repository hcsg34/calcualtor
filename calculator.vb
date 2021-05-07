Module module1

    Sub main()

        Console.CursorVisible = False
        menu() 'calls to the main menu
    End Sub
    Sub pointer() 'for writing the menu
        Console.ForegroundColor = ConsoleColor.Yellow
        Console.Write("-->")
        Console.ForegroundColor = ConsoleColor.White
    End Sub
    Sub clearRow(ByVal howMany As Integer) 'function to clear lines for formatting
        Console.Write(StrDup(howMany, " "))
        Console.SetCursorPosition(Console.CursorLeft - howMany, Console.CursorTop)
    End Sub
    Sub writemenu(ByVal choice As Integer, ByVal options() As String, ByVal previous As Integer) 'function to call to display the menu
        Console.SetCursorPosition(0, previous) 'previous is last location of string - allows the menu to refresh faster
        clearRow(50)
        Console.SetCursorPosition(0, previous)
        Console.WriteLine(options(previous - 1))
        'Console.Clear()

        For i = 1 To options.Length
            Console.SetCursorPosition(0, i)

            If i = choice Then 'if current option is selected
                pointer()
                Console.BackgroundColor = ConsoleColor.DarkGreen
                Console.ForegroundColor = ConsoleColor.Black
            End If
            Console.WriteLine(options(i - 1))
            Console.BackgroundColor = ConsoleColor.Black
            Console.ForegroundColor = ConsoleColor.White
        Next
    End Sub
    Sub menu()
        Dim options() As String = {"Add",
        "Subtract",
        "Multiply",
        "Calculate Determinant",
        "Solve Sets of Simultaneous Equations",
        "Create Questions",
        "Exit"} 'uses array to pass through write function
        Dim choice As Integer = 1
        Dim previous As Integer = 1
        Dim key As ConsoleKey 'uses console key to allow me to only allow certain inputs easily
        Do
            Console.ForegroundColor = ConsoleColor.Red
            Console.Write("Use Arrow Keys and Enter Button to Select Choice")
            Console.ForegroundColor = ConsoleColor.White
            Do

                writemenu(choice, options, previous) 'outputs menu
                previous = choice 'stores last choice
                key = Console.ReadKey(True).Key 'reads key
                Select Case key
                    Case ConsoleKey.DownArrow
                        If choice < options.Length Then
                            choice += 1
                        End If

                    Case ConsoleKey.UpArrow
                        If choice > 1 Then choice -= 1
                End Select
            Loop Until key = ConsoleKey.Enter
            Console.Clear()
            Select Case choice
                Case 1
                    Add()
                Case 2
                    Subtract()
                Case 3
                    Multiply()
                Case 4
                    determinant()
                Case 5
                    simultaneousEquations()
                Case 6
                    createQuestion()
            End Select
            Console.ReadKey()
            Console.Clear()
        Loop Until choice = options.Length 'loop until clicks enter on exit
    End Sub
    Function enterDimensions(ByVal min As Integer, ByVal max As Integer, ByVal name As String) As Integer 'choosing dimenstions for matrix
        Console.WriteLine()
        Dim temp As Integer
        Do
            Try
                clearRow(50)
                Console.SetCursorPosition(0, Console.CursorTop)
                Console.Write(name & ": ")
                temp = Console.ReadLine
                If temp < min Or temp > max Then
                    Throw New ArgumentOutOfRangeException("Outside of range") 'custom error for correct range
                End If
                Return temp
            Catch e As System.InvalidCastException
                Console.SetCursorPosition(0, Console.CursorTop - 2)
                clearRow(50)
                Console.ForegroundColor = ConsoleColor.Red
                Console.WriteLine("Please enter a number")
                Console.ForegroundColor = ConsoleColor.White

            Catch e As System.ArgumentOutOfRangeException
                Console.SetCursorPosition(0, Console.CursorTop - 2)
                clearRow(50)
                Console.ForegroundColor = ConsoleColor.Red
                Console.WriteLine("Enter a number in the range: " & min & " - " & max)
                Console.ForegroundColor = ConsoleColor.White

            Catch e As System.Exception
                MsgBox("Unknown Error")
            End Try 'keeps trying until correct type in correct range
        Loop
    End Function
    Sub simultaneousEquations() 'Guassian elimination to solve sets of simultaneous linear equations
        Dim flag As Integer = 0 '0 intersects, 1 is infinite, 2 is no sols
        Dim height As Integer = enterDimensions(2, 4, "Enter Number of Equations") - 1 'always has 1 less row than columb for augemnted matrix
        Dim matrix1 As matrix = SetUpMatrix(height + 1, height, 1, True) 'sets matrix
        Dim matrix2 As matrix = matrix1.clone 'clones to prevent errors

        matrix2.bubbleSort() 'bubble sort based on total absolute values of rows to put any 0 rows at the bottom

        If Not matrix2.GaussJordanElimination() Then flag = 1 'GuassJordanElimination returns true on success so this means it failed
        For i = 0 To matrix2.getHeight
            If checkZeros(matrix2, matrix2.getWidth - 1, i) Then
                If matrix2.getVal(matrix2.getWidth, i) <> 0 Then flag = 2 'if any non-0 rows then there are no solutions
            End If
        Next
        Console.ForegroundColor = ConsoleColor.Red
        Console.WriteLine("ORIGINAL MATRIX")
        Console.ForegroundColor = ConsoleColor.White
        matrix1.output()
        Console.ForegroundColor = ConsoleColor.Red
        Console.WriteLine("END MATRIX")
        Console.ForegroundColor = ConsoleColor.White
        matrix2.output()
        Select Case flag
            Case 0
                Console.ForegroundColor = ConsoleColor.Red
                Console.WriteLine("SOLUTIONS")
                Console.ForegroundColor = ConsoleColor.White
                For i = 0 To matrix2.getHeight
                    Console.WriteLine("x" & i + 1 & "  =  " & Math.Round(matrix2.getVal(matrix2.getWidth, i), 5)) 'rounds for easier viewing, RRE form makes it so each row has the corresponding val
                Next
            Case 1
                Console.WriteLine("Infinite solutions")
            Case 2
                Console.WriteLine("No Solutions")
        End Select


    End Sub
    Function checkZeros(ByVal matrix1 As matrix, ByVal width As Integer, ByVal row As Integer) As Boolean  'if whole row not 0 then return false
        For i = 0 To width
            If matrix1.getVal(i, row) <> 0 Then Return False
        Next
        Return True
    End Function
    Sub determinant()
        Console.ForegroundColor = ConsoleColor.Red
        Console.WriteLine("Determinant can only be found for square matrices")
        Console.ForegroundColor = ConsoleColor.White
        Dim width As Integer = enterDimensions(1, 10, "Size of Matrix") - 1 'get dimensions
        Dim matrix1 As matrix = SetUpMatrix(width, width, 0, False) 'set up matrix
        Console.Clear()
        Console.ForegroundColor = ConsoleColor.Red
        Console.WriteLine("MATRIX: ")
        Console.ForegroundColor = ConsoleColor.White
        matrix1.output()

        Console.ForegroundColor = ConsoleColor.Red
        Console.Write(vbCrLf & "DET =")
        Console.ForegroundColor = ConsoleColor.White
        Console.WriteLine(matrix1.det) 'calculates det
    End Sub
    Sub Multiply()
        Console.WriteLine("Matrix 1")
        Dim width As Integer = enterDimensions(1, 10, "width") - 1
        Dim height As Integer = enterDimensions(1, 10, "height") - 1
        Console.Clear()
        Console.ForegroundColor = ConsoleColor.Red
        Console.WriteLine("Height autofills due to matrix rules")
        Console.ForegroundColor = ConsoleColor.White
        Console.WriteLine("Matrix 2")
        Dim width1 As Integer = enterDimensions(1, 10, "width") - 1
        Console.Write(vbCrLf & "Height: " & width + 1)
        Console.ReadKey()
        Dim matrix1 As matrix = SetUpMatrix(width, height, 1, False)
        Dim matrix2 As matrix = SetUpMatrix(width1, width, 2, False)
        Dim matrix3 As matrix = matrix1.multiply(matrix2)
        Console.ForegroundColor = ConsoleColor.Red
        Console.WriteLine("MATRIX 1")
        Console.ForegroundColor = ConsoleColor.White
        matrix1.output()
        Console.ForegroundColor = ConsoleColor.Red
        Console.WriteLine("MATRIX 2")
        Console.ForegroundColor = ConsoleColor.White
        matrix2.output()
        Console.ForegroundColor = ConsoleColor.Red
        Console.WriteLine("MATRIX 1 X MATRIX 2")
        Console.ForegroundColor = ConsoleColor.White
        matrix3.output()
    End Sub
    Function randomMatrix(ByVal width As Integer, ByVal height As Integer, ByVal range As Integer) As Double(,)
        Randomize()
        Dim nums(width, height) As Double 'matrix
        For i = 0 To height
            For j = 0 To width

                nums(j, i) = CInt(Rnd() * range - (range \ 2)) 'generates a number between positive half range and negative half the range
            Next
        Next
        Return nums
    End Function
    Sub createAdd()
        Dim width As Integer = enterDimensions(1, 10, "width") - 1
        Dim height As Integer = enterDimensions(1, 10, "height") - 1

        Dim matrix1 As New matrix(width, height, randomMatrix(width, height, 100), False) 'creates random matrix
        Dim matrix2 As New matrix(width, height, randomMatrix(width, height, 100), False) 'creates random matrix
        Console.Clear()
        Console.ForegroundColor = ConsoleColor.DarkGreen
        Console.WriteLine("ADD: ")
        Console.ForegroundColor = ConsoleColor.Blue
        Console.WriteLine("Matrix 1 + Matrix 2")
        Console.ForegroundColor = ConsoleColor.Red
        Console.WriteLine("Matrix 1")
        Console.ForegroundColor = ConsoleColor.White
        matrix1.output()
        Console.ForegroundColor = ConsoleColor.Red
        Console.WriteLine("Matrix 2")
        Console.ForegroundColor = ConsoleColor.White
        matrix2.output()
        matrix1.add(matrix2, True) 'adds and then outputs
        answerOutput(matrix1, Console.CursorTop)
    End Sub
    Sub createMultiply()
        Console.WriteLine("Matrix 1")
        Dim width As Integer = enterDimensions(1, 10, "width") - 1
        Dim height As Integer = enterDimensions(1, 10, "height") - 1
        Console.Clear()
        Console.ForegroundColor = ConsoleColor.Red
        Console.WriteLine("Height autofills due to matrix rules")
        Console.ForegroundColor = ConsoleColor.White
        Console.WriteLine("Matrix 2")
        Dim width1 As Integer = enterDimensions(1, 10, "width") - 1
        Console.Write(vbCrLf & "Height: " & width + 1)
        Console.ReadKey()
        Dim matrix1 As New matrix(width, height, randomMatrix(width, height, 22), False) 'creates random matrix
        Dim matrix2 As New matrix(width1, width, randomMatrix(width1, width, 22), False) 'creates random matrix
        Console.Clear()
        Console.ForegroundColor = ConsoleColor.DarkGreen
        Console.WriteLine("MULTIPLY: ")
        Console.ForegroundColor = ConsoleColor.Blue
        Console.WriteLine("Matrix 1 X Matrix 2")
        Console.ForegroundColor = ConsoleColor.Red
        Console.WriteLine("Matrix 1")
        Console.ForegroundColor = ConsoleColor.White
        matrix1.output()
        Console.ForegroundColor = ConsoleColor.Red
        Console.WriteLine("Matrix 2")
        Console.ForegroundColor = ConsoleColor.White
        matrix2.output()
        Dim matrix3 As matrix = matrix1.multiply(matrix2) 'need new matrix to stop errors
        answerOutput(matrix3, Console.CursorTop + 2) 'outputs
    End Sub
    Sub createSubtract()
        Dim width As Integer = enterDimensions(1, 10, "width") - 1
        Dim height As Integer = enterDimensions(1, 10, "height") - 1

        Dim matrix1 As New matrix(width, height, randomMatrix(width, height, 100), False) 'creates random matrix
        Dim matrix2 As New matrix(width, height, randomMatrix(width, height, 100), False) 'creates random matrix
        Console.Clear()
        Console.ForegroundColor = ConsoleColor.DarkGreen
        Console.WriteLine("SUBTRACT: ")
        Console.ForegroundColor = ConsoleColor.Blue
        Console.WriteLine("Matrix 1 - Matrix 2")
        Console.ForegroundColor = ConsoleColor.Red
        Console.WriteLine("Matrix 1")
        Console.ForegroundColor = ConsoleColor.White
        matrix1.output()
        Console.ForegroundColor = ConsoleColor.Red
        Console.WriteLine("Matrix 2")
        Console.ForegroundColor = ConsoleColor.White
        matrix2.output()
        matrix1.add(matrix2, False)
        answerOutput(matrix1, Console.CursorTop)
    End Sub
    Sub createSimEq() 'had to copy paste code from earlier in here as wouldnt work with passing it through
        Dim flag As Integer = 0 '0 intersects, 1 is infinite, 2 is no sols
        Dim height As Integer = enterDimensions(2, 4, "Enter Number of Equations") - 1
        Dim matrix1 As New matrix(height + 1, height, randomMatrix(height + 1, height, 22), True) 'creates random matrix
        Dim matrix2 As matrix = matrix1.clone

        matrix2.bubbleSort()

        If Not matrix2.GaussJordanElimination() Then flag = 1
        For i = 0 To matrix2.getHeight
            If checkZeros(matrix2, matrix2.getWidth - 1, i) Then
                If matrix2.getVal(matrix2.getWidth, i) <> 0 Then flag = 2
            End If
        Next
        Dim text As String = ""
        Dim sols(matrix2.getHeight) As Double
        Select Case flag
            Case 0
                For i = 0 To matrix2.getHeight
                    sols(i) = Math.Round(matrix2.getVal(matrix2.getWidth, i), 5)
                Next
            Case 1
                text = ("Infinite solutions")
            Case 2
                text = ("No Solutions")
        End Select
        Console.Clear()
        Console.ForegroundColor = ConsoleColor.DarkGreen
        Console.Write("FIND SOLUTIONS OF: ")
        Console.ForegroundColor = ConsoleColor.Blue
        Console.WriteLine("Matrix 1")
        Console.ForegroundColor = ConsoleColor.Red
        Console.ForegroundColor = ConsoleColor.Red
        Console.WriteLine("Matrix ")
        Console.ForegroundColor = ConsoleColor.White
        matrix1.output()
        matrix2.roundVals(3)
        answerOutputSim(matrix2, sols, Console.CursorTop, text) 'solves and outputs


    End Sub
    Sub createDet()
        Dim width As Integer = enterDimensions(1, 10, "size") - 1
        Dim matrix1 As New matrix(width, width, randomMatrix(width, width, 22), False) 'creates random matrix
        Dim temp As Double(,) = {{matrix1.det}} 'answer output requires matrix to output so creates 1x1 matrix with number
        Dim matrix2 As New matrix(0, 0, temp, False)
        Console.Clear()
        Console.ForegroundColor = ConsoleColor.DarkGreen
        Console.Write("CALCULATE DETERMINANT OF: ")
        Console.ForegroundColor = ConsoleColor.Blue
        Console.WriteLine("Matrix 1")
        Console.ForegroundColor = ConsoleColor.Red
        Console.WriteLine("Matrix")
        Console.ForegroundColor = ConsoleColor.White
        matrix1.output()
        answerOutput(matrix2, Console.CursorTop)
    End Sub
    Sub answerOutputSim(ByVal matrix1 As matrix, ByVal sols As Double(), ByVal startRow As Integer, ByVal text As String)
        Dim key As ConsoleKey
        Dim choice As Integer = 0
        Dim showing As Boolean = False 'to close and open answer
        Do
            Do
                Console.SetCursorPosition(0, startRow)
                clearRow(50)
                If choice = 0 Then
                    pointer()
                    Console.BackgroundColor = ConsoleColor.DarkGreen
                    Console.ForegroundColor = ConsoleColor.Black
                End If

                If Not showing Then
                    Console.WriteLine("Show Answer")
                Else
                    Console.WriteLine("Hide Answer")
                End If
                Console.BackgroundColor = ConsoleColor.Black
                Console.ForegroundColor = ConsoleColor.White
                clearRow(50)
                If choice = 1 Then
                    pointer()
                    Console.BackgroundColor = ConsoleColor.DarkGreen
                    Console.ForegroundColor = ConsoleColor.Black
                End If
                Console.WriteLine("Exit")
                Console.BackgroundColor = ConsoleColor.Black
                Console.ForegroundColor = ConsoleColor.White
                key = Console.ReadKey(True).Key
                If key = ConsoleKey.DownArrow Or key = ConsoleKey.UpArrow Then
                    choice = 1 - choice
                End If
            Loop Until key = ConsoleKey.Enter
            If choice = 0 Then
                showing = Not showing
                If Not showing Then
                    Console.SetCursorPosition(0, startRow)
                    For i = 2 To sols.Length + matrix1.getHeight + 8
                        Console.WriteLine("")
                        clearRow(100)
                    Next
                Else
                    Console.SetCursorPosition(0, startRow + 2)
                    Console.ForegroundColor = ConsoleColor.Red
                    Console.WriteLine(vbCrLf & vbCrLf & "ANSWER")
                    Console.ForegroundColor = ConsoleColor.White
                    matrix1.output()
                    Console.ForegroundColor = ConsoleColor.Blue
                    Console.WriteLine(text)
                    Console.ForegroundColor = ConsoleColor.White
                    If text = "" Then

                        For i = 0 To sols.Length - 1
                            Console.WriteLine("X" & i & " = " & sols(i))
                        Next
                    Else
                        Console.WriteLine(text)
                    End If
                End If
            End If
        Loop Until choice = 1

    End Sub
    Sub answerOutput(ByVal matrix1 As matrix, ByVal startRow As Integer)
        Dim key As ConsoleKey
        Dim choice As Integer = 0
        Dim showing As Boolean = False
        Do
            Do
                Console.SetCursorPosition(0, startRow)
                clearRow(50)
                If choice = 0 Then
                    pointer()
                    Console.BackgroundColor = ConsoleColor.DarkGreen
                    Console.ForegroundColor = ConsoleColor.Black
                End If

                If Not showing Then
                    Console.WriteLine("Show Answer")
                Else
                    Console.WriteLine("Hide Answer")
                End If
                Console.BackgroundColor = ConsoleColor.Black
                Console.ForegroundColor = ConsoleColor.White
                clearRow(50)
                If choice = 1 Then
                    pointer()
                    Console.BackgroundColor = ConsoleColor.DarkGreen
                    Console.ForegroundColor = ConsoleColor.Black
                End If
                Console.WriteLine("Exit")
                Console.BackgroundColor = ConsoleColor.Black
                Console.ForegroundColor = ConsoleColor.White
                key = Console.ReadKey(True).Key
                If key = ConsoleKey.DownArrow Or key = ConsoleKey.UpArrow Then
                    choice = 1 - choice
                End If
            Loop Until key = ConsoleKey.Enter
            If choice = 0 Then
                showing = Not showing
                If Not showing Then
                    Console.SetCursorPosition(0, startRow)
                    For i = 2 To matrix1.getHeight + 6
                        Console.WriteLine("")
                        clearRow(100)
                    Next
                Else
                    Console.SetCursorPosition(0, startRow + 2)
                    Console.ForegroundColor = ConsoleColor.Red
                    Console.WriteLine(vbCrLf & vbCrLf & "ANSWER")
                    Console.ForegroundColor = ConsoleColor.White
                    matrix1.output()
                End If
            End If
        Loop Until choice = 1

    End Sub
    Sub Add()
        Dim width As Integer = enterDimensions(1, 10, "width") - 1
        Dim height As Integer = enterDimensions(1, 10, "height") - 1
        Dim matrix1 As matrix = SetUpMatrix(width, height, 1, False)
        Dim matrix2 As matrix = SetUpMatrix(width, height, 2, False)
        Dim matrix3 As matrix = matrix1.clone
        'matrix1.output()
        'matrix2.output()
        matrix3.add(matrix2, True)
        Console.ForegroundColor = ConsoleColor.Red
        Console.WriteLine("MATRIX 1")
        Console.ForegroundColor = ConsoleColor.White
        matrix1.output()
        Console.ForegroundColor = ConsoleColor.Red
        Console.WriteLine("MATRIX 2")
        Console.ForegroundColor = ConsoleColor.White
        matrix2.output()
        Console.ForegroundColor = ConsoleColor.Red
        Console.WriteLine("MATRIX 1 + MATRIX 2")
        Console.ForegroundColor = ConsoleColor.White
        matrix3.output()
    End Sub
    Sub Subtract()
        Dim width As Integer = enterDimensions(1, 10, "width") - 1
        Dim height As Integer = enterDimensions(1, 10, "height") - 1
        Dim matrix1 As matrix = SetUpMatrix(width, height, 1, False)
        Dim matrix2 As matrix = SetUpMatrix(width, height, 2, False)
        Dim matrix3 As matrix = matrix1.clone
        'matrix1.output()
        'matrix2.output()
        matrix3.add(matrix2, False)

        Console.ForegroundColor = ConsoleColor.Red
        Console.WriteLine("MATRIX 1")
        Console.ForegroundColor = ConsoleColor.White
        matrix1.output()
        Console.ForegroundColor = ConsoleColor.Red
        Console.WriteLine("MATRIX 2")
        Console.ForegroundColor = ConsoleColor.White
        matrix2.output()
        Console.ForegroundColor = ConsoleColor.Red
        Console.WriteLine("MATRIX 1 - MATRIX 2")
        Console.ForegroundColor = ConsoleColor.White
        matrix3.output()
    End Sub
    Function SetUpMatrix(ByVal width As Integer, ByVal height As Integer, ByVal n As Integer, ByVal augmented As Boolean) As matrix

        Dim nums(width, height) As String
        Dim numNums(width, height) As Double
        For i = 0 To height
            For j = 0 To width
                nums(j, i) = "+0"
            Next
        Next
        Dim choice() As Integer = {0, 0}
        Dim previous() As Integer = choice
        Dim key As ConsoleKey
        Dim done As Boolean = False
        Dim spacing As Integer = 4
        Console.Clear()
        Console.WriteLine("Enter matrix " & n & ":")
        writeMatrix(nums, augmented, choice, spacing, previous)
        Do
            key = Console.ReadKey(True).Key
            previous = choice
            Select Case key
                Case ConsoleKey.OemPeriod
                    If Not nums(choice(0), choice(1)).Contains(".") Then
                        nums(choice(0), choice(1)) = nums(choice(0), choice(1)) & "."
                    End If
                Case 189
                    nums(choice(0), choice(1)) = "-" & nums(choice(0), choice(1)).Substring(1)
                Case 187
                    nums(choice(0), choice(1)) = "+" & nums(choice(0), choice(1)).Substring(1)
                Case ConsoleKey.Backspace
                    If Len(nums(choice(0), choice(1))) > 2 Then
                        nums(choice(0), choice(1)) = nums(choice(0), choice(1)).Substring(0, nums(choice(0), choice(1)).Length - 1)
                    Else
                        nums(choice(0), choice(1)) = nums(choice(0), choice(1))(0) & "0"
                    End If
                Case ConsoleKey.Enter
                    If choice(0) = width And choice(1) = height Then
                        done = True
                    ElseIf choice(0) <> width Then
                        choice(0) += 1
                    Else
                        choice(0) = 0
                        choice(1) += 1
                    End If
                Case ConsoleKey.RightArrow
                    If choice(0) = width And choice(1) = height Then
                        choice(0) = 0
                        choice(1) = 0
                    ElseIf choice(0) = width Then
                        choice(0) = 0
                        choice(1) += 1
                    Else
                        choice(0) += 1
                    End If
                Case ConsoleKey.LeftArrow
                    If choice(0) = 0 And choice(1) = 0 Then
                        choice(0) = width
                        choice(1) = height
                    ElseIf choice(0) = 0 Then
                        choice(0) = width
                        choice(1) -= 1
                    Else
                        choice(0) -= 1
                    End If
                Case ConsoleKey.DownArrow
                    If choice(1) = height Then
                        choice(1) = 0
                    Else
                        choice(1) += 1
                    End If
                Case ConsoleKey.UpArrow
                    If choice(1) = 0 Then
                        choice(1) = height
                    Else
                        choice(1) -= 1
                    End If
                Case Else
                    If nums(choice(0), choice(1)).Length < 7 Then
                        key = key - 48
                        If key >= 0 And key <= 9 Then
                            If nums(choice(0), choice(1))(1) = "0" And nums(choice(0), choice(1)).Length = 2 Then
                                nums(choice(0), choice(1)) = nums(choice(0), choice(1))(0)
                            End If
                            nums(choice(0), choice(1)) = nums(choice(0), choice(1)) & key
                        End If
                        If nums(choice(0), choice(1)).Length > spacing - 1 Then spacing += 1
                    End If
            End Select

            If Not done Then writeMatrix(nums, augmented, choice, spacing, previous)


        Loop Until done = True
        For i = 0 To height
            For j = 0 To width
                numNums(j, i) = CDbl(nums(j, i))
            Next
        Next
        Console.Clear()
        Dim matrix As New matrix(width, height, numNums, augmented)
        Return matrix
    End Function
    Sub writeMatrix(ByVal nums(,) As String, ByVal augmented As Boolean, ByVal choice() As Integer, ByVal spacing As Integer, ByVal previous() As Integer)
        Console.SetCursorPosition(0, 1)
        For i = 0 To nums.GetLength(1) - 1 '1 is no of rows
            For j = 0 To nums.GetLength(0) - 1 '0 is no of columns
                If j = choice(0) And i = choice(1) Then

                    Console.BackgroundColor = ConsoleColor.DarkGreen
                    Console.ForegroundColor = ConsoleColor.Black

                    Console.Write(nums(j, i))
                    Console.BackgroundColor = ConsoleColor.Black
                    Console.ForegroundColor = ConsoleColor.White
                ElseIf j = previous(0) And i = previous(1) Then
                    clearRow(spacing)
                    Console.Write(nums(j, i))
                Else

                    Console.Write(nums(j, i))
                End If




                Console.Write(StrDup(CInt(spacing - Str(nums(j, i).Length)), " "))
                If augmented = True AndAlso j = nums.GetLength(0) - 2 Then Console.Write("| ")
            Next
            Console.WriteLine()
        Next

    End Sub
    Sub createQuestion()
        Dim options() As String = {"Add",
        "Subtract",
        "Multiply",
        "Determinant",
        "Simultaneous Equations Solver",
        "Exit"}
        Dim choice As Integer = 1
        Dim previous As Integer = 1
        Dim key As ConsoleKey
        If Console.KeyAvailable Then
            Console.ReadKey()
        End If
        Do
            Console.ForegroundColor = ConsoleColor.Red
            Console.WriteLine("Enter Type of Question to Create")
            Console.ForegroundColor = ConsoleColor.White
            Do
                writemenu(choice, options, previous)
                previous = choice
                key = Console.ReadKey(True).Key
                Select Case key
                    Case ConsoleKey.DownArrow
                        If choice < options.Length Then
                            choice += 1
                        End If

                    Case ConsoleKey.UpArrow
                        If choice > 1 Then choice -= 1
                End Select
            Loop Until key = ConsoleKey.Enter
            Console.Clear()
            Select Case choice
                Case 1
                    createAdd()
                Case 2
                    createSubtract()
                Case 3
                    createMultiply()
                Case 4
                    createDet()
                Case 5
                    createSimEq()
            End Select
            Console.Clear()
        Loop Until choice = options.Length
    End Sub
End Module



Public Class matrix
    Protected grid(,) As Double
    Protected width As Integer
    Protected height As Integer
    Private augmented As Boolean

    Public Sub New(ByVal inx As Integer, ByVal iny As Integer)
        width = inx
        height = iny
        ReDim grid(width, height)
    End Sub
    Public Sub New(ByVal inx As Integer, ByVal iny As Integer, ByVal arr(,) As Double, ByVal inAug As Boolean)
        width = inx
        height = iny
        ReDim grid(width, height)
        For j = 0 To height
            For i = 0 To width
                grid(i, j) = arr(i, j)
            Next
        Next
        augmented = inAug
    End Sub
    Public Function getWidth() As Integer
        Return width
    End Function
    Public Function getHeight() As Integer
        Return height
    End Function
    Public Function getVal(ByVal inx As Integer, ByVal iny As Integer) As Double
        Return grid(inx, iny)
    End Function

    Public Sub setVal(ByVal x As Integer, ByVal y As Integer, ByVal val As Double)
        grid(x, y) = val
    End Sub
    Public Sub add(ByVal matrix As matrix, ByVal pos As Boolean)
        If pos = True Then

            For i = 0 To width
                For j = 0 To height
                    grid(i, j) = grid(i, j) + matrix.getVal(i, j)
                Next
            Next
        Else
            For i = 0 To width
                For j = 0 To height
                    grid(i, j) = grid(i, j) - matrix.getVal(i, j)
                Next
            Next
        End If
    End Sub
    Public Function multiply(ByVal matrix As matrix) As matrix
        Dim numbers(matrix.width, height) As Double
        For i = 0 To matrix.width
            For j = 0 To height
                numbers(i, j) = 0 'new grid w numbers in
            Next
        Next

        For i = 0 To matrix.width
            For j = 0 To height
                For k = 0 To width
                    numbers(i, j) += grid(k, j) * matrix.getVal(i, k)

                Next
            Next
        Next
        Dim temp As New matrix(matrix.width, height, numbers, False)
        Return temp
    End Function

    Public Function det() As Double
        Dim cX As Integer = 0
        Dim cY As Integer = 0
        Dim cSize As Integer = width
        Dim val As Integer = 0 'val of det
        Dim nums As New List(Of List(Of Double))
        For i = 0 To width
            nums.Add(New List(Of Double))
            For j = 0 To width
                nums(i).Add(grid(i, j))
            Next
        Next
        Return detRecursive(cSize, nums)
    End Function
    Public Function detRecursive(ByVal csize As Integer, ByVal nums As List(Of List(Of Double))) As Double
        Dim val As Double = 0
        Dim temp As New List(Of List(Of Double))
        temp = cloneList(nums)
        If csize = 0 Then
            Return nums(0)(0)
        ElseIf csize = 1 Then
            Return Math.Round(nums(0)(0) * nums(1)(1) - nums(0)(1) * nums(1)(0), 15)
        Else
            For i = 0 To csize
                temp = cloneList(nums)
                temp.RemoveAt(0)
                For j = 0 To csize - 1
                    temp(j).RemoveAt(i)
                Next
                If i / 2 = i \ 2 Then
                    val += nums(i)(0) * detRecursive(csize - 1, temp)
                Else
                    val -= nums(i)(0) * detRecursive(csize - 1, temp)
                End If
            Next
        End If
        Return Math.Round(val, 15)
    End Function
    Public Function cloneList(ByVal nums As List(Of List(Of Double))) As List(Of List(Of Double)) 'only works 
        Dim tempo As New List(Of List(Of Double))
        For i = 0 To nums.Count - 1
            tempo.Add(New List(Of Double))
            For j = 0 To nums(0).Count - 1
                tempo(i).Add(nums(j)(i))
            Next
        Next
        Return tempo
    End Function
    '''''''''''''''''''''''''''''''
    '  Elementary Row Operations  '
    '''''''''''''''''''''''''''''''
    Public Sub swapRows(ByVal row1 As Integer, ByVal row2 As Integer)
        Dim temp As Double
        For i = 0 To width
            temp = grid(i, row1)
            grid(i, row1) = grid(i, row2)
            grid(i, row2) = temp
        Next
    End Sub
    Public Sub multiplyRow(ByVal row1 As Integer, ByVal multiplier As Double)
        For i = 0 To width
            grid(i, row1) = Math.Round(grid(i, row1) * multiplier, 15)
        Next
    End Sub
    Public Sub addRowToRow(ByVal row1 As Integer, ByVal row2 As Integer, ByVal multiplier As Double)
        For i = 0 To width
            grid(i, row1) += grid(i, row2) * multiplier
        Next
    End Sub
    '''''''''''''''''''''''''''''''
    '              END            '
    '''''''''''''''''''''''''''''''
    Public Function clone() As matrix
        Dim arr(width, height) As Double
        For i = 0 To height
            For j = 0 To width
                arr(j, i) = grid(j, i)
            Next
        Next
        Return New matrix(width, height, arr, augmented)
    End Function

    Public Sub bubbleSort()
        Dim temp As Integer
        Dim val(height) As Integer
        For i = 0 To height
            For j = 0 To width
                val(i) += Math.Abs(grid(j, i))
            Next
        Next
        For j = 0 To height - 1
            For i = 0 To height - 1 - j
                If val(i) < val(i + 1) Then
                    temp = val(i)
                    val(i) = val(i + 1)
                    val(i + 1) = temp
                    Me.swapRows(i, i + 1)
                End If
            Next
        Next
    End Sub
    Public Function zeroColumn(ByVal k As Integer) As Boolean
        For i = 0 To height
            If grid(k, i) <> 0 Then Return False
        Next
        Return True
    End Function
    Public Function zerorow(ByVal k As Integer) As Boolean
        For i = 0 To width - 1
            If grid(i, k) <> 0 Then Return False
        Next
        Return True
    End Function
    Public Function GaussJordanElimination() As Boolean
        Dim tempMatrix As New matrix(width - 1, height, Me.deAugment(), False)

        Dim n As Integer = width 'order of matrix
        Dim m As Integer = height
        Dim h As Integer = 0 'row
        Dim k As Integer = 0 'column
        Dim maxVal As Double = 0
        'Swap the rows so that all rows with all zero entries are on the bottom
        '  Me.bubbleSort()
        While k <= height And h <= height
            While k <= height AndAlso Me.zeroColumn(k) = True
                k += 1
            End While
            While h <= height AndAlso Me.zerorow(h) = True
                h += 1
            End While
            If k <= height AndAlso h <= height AndAlso grid(k, h) = 0 Then
                For i = k + 1 To height
                    If grid(i, h) <> 0 Then Me.swapRows(k, i)
                Next
            End If


            If h > height Or k > height Then
                Return False
            End If
            Me.multiplyRow(h, 1 / grid(k, h))
            For i = 0 To height
                If i <> h Then Me.addRowToRow(i, h, -grid(k, i))
            Next
            h += 1
            k += 1

        End While
        If tempMatrix.det = 0 Then Return False
        Return True
    End Function

    Private Function deAugment() As Double(,)
        Dim temp(width - 1, height) As Double
        For j = 0 To height
            For i = 0 To width - 1
                temp(i, j) = grid(i, j)
            Next
        Next
        Return temp
    End Function
    Public Sub roundVals(ByVal DP As Integer)
        For i = 0 To width
            For j = 0 To height
                grid(i, j) = Math.Round(grid(i, j), DP)
            Next
        Next
    End Sub
    Public Sub output()
        Dim spacing As Integer = 2
        For i = 0 To height
            For j = 0 To width
                If CStr(grid(j, i)).Length > spacing Then spacing = CStr(grid(j, i)).Length
            Next
        Next
        spacing += 2
        For i = 0 To height '1 is no of rows

            For j = 0 To width '0 is no of columns
                Console.Write(grid(j, i))
                Console.Write(StrDup(CInt(spacing - CStr(grid(j, i)).Length), " "))
                If augmented AndAlso j = width - 1 Then Console.Write("| ")
            Next
            Console.WriteLine()
        Next

    End Sub
End Class














