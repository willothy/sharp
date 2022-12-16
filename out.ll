; ModuleID = 'main'
source_filename = "main"

%node = type { i32, %node* }

@str = private unnamed_addr constant [10 x i8] c"head: %d\0A\00", align 1
@str.1 = private unnamed_addr constant [15 x i8] c"head.next: %d\0A\00", align 1
@str.2 = private unnamed_addr constant [20 x i8] c"head.next.next: %d\0A\00", align 1
@str.3 = private unnamed_addr constant [7 x i8] c"i: %d\0A\00", align 1

declare i32 @printf(i8*, ...)

declare %node* @malloc(i64)

declare void @free(%node*)

define %node* @new_node(i32 %0) {
entry:
  %ptr = alloca %node*, align 8
  %calltmp = call %node* @malloc(i64 ptrtoint (%node* getelementptr (%node, %node* null, i32 1) to i64))
  store %node* %calltmp, %node** %ptr, align 8
  %ptr1 = load %node*, %node** %ptr, align 8
  %node.value = getelementptr inbounds %node, %node* %ptr1, i32 0, i32 0
  store i32 %0, i32* %node.value, align 4
  %deref_assign_result = load i32, i32* %node.value, align 4
  %ptr2 = load %node*, %node** %ptr, align 8
  ret %node* %ptr2
}

define %node* @push_front(%node* %0, i32 %1) {
entry:
  %ptr = alloca %node*, align 8
  %calltmp = call %node* @new_node(i32 %1)
  store %node* %calltmp, %node** %ptr, align 8
  %ptr1 = load %node*, %node** %ptr, align 8
  %node.next = getelementptr inbounds %node, %node* %ptr1, i32 0, i32 1
  store %node* %0, %node** %node.next, align 8
  %deref_assign_result = load %node*, %node** %node.next, align 8
  %ptr2 = load %node*, %node** %ptr, align 8
  ret %node* %ptr2
}

define void @node_test() {
entry:
  %head = alloca %node*, align 8
  %calltmp = call %node* @new_node(i32 1)
  store %node* %calltmp, %node** %head, align 8
  %a = alloca i32, align 4
  store i32 2, i32* %a, align 4
  %b = alloca i32, align 4
  store i32 3, i32* %b, align 4
  %head1 = load %node*, %node** %head, align 8
  %a2 = load i32, i32* %a, align 4
  %calltmp3 = call %node* @push_front(%node* %head1, i32 %a2)
  store %node* %calltmp3, %node** %head, align 8
  %head4 = load %node*, %node** %head, align 8
  %head5 = load %node*, %node** %head, align 8
  %b6 = load i32, i32* %b, align 4
  %calltmp7 = call %node* @push_front(%node* %head5, i32 %b6)
  store %node* %calltmp7, %node** %head, align 8
  %head8 = load %node*, %node** %head, align 8
  %head9 = load %node*, %node** %head, align 8
  %value = getelementptr inbounds %node, %node* %head9, i32 0, i32 0
  %deref = load i32, i32* %value, align 4
  %calltmp10 = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([10 x i8], [10 x i8]* @str, i32 0, i32 0), i32 %deref)
  %head11 = load %node*, %node** %head, align 8
  %next = getelementptr inbounds %node, %node* %head11, i32 0, i32 1
  %deref12 = load %node*, %node** %next, align 8
  %value13 = getelementptr inbounds %node, %node* %deref12, i32 0, i32 0
  %deref14 = load i32, i32* %value13, align 4
  %calltmp15 = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([15 x i8], [15 x i8]* @str.1, i32 0, i32 0), i32 %deref14)
  %head16 = load %node*, %node** %head, align 8
  %next17 = getelementptr inbounds %node, %node* %head16, i32 0, i32 1
  %deref18 = load %node*, %node** %next17, align 8
  %next19 = getelementptr inbounds %node, %node* %deref18, i32 0, i32 1
  %deref20 = load %node*, %node** %next19, align 8
  %value21 = getelementptr inbounds %node, %node* %deref20, i32 0, i32 0
  %deref22 = load i32, i32* %value21, align 4
  %calltmp23 = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([20 x i8], [20 x i8]* @str.2, i32 0, i32 0), i32 %deref22)
  ret void
}

define i32 @main() {
entry:
  %i = alloca i32, align 4
  store i32 0, i32* %i, align 4
  br label %loop_entry

loop_entry:                                       ; preds = %entry
  br label %loop_body

loop_body:                                        ; preds = %end, %loop_entry
  %i1 = load i32, i32* %i, align 4
  %isubtmp = icmp eq i32 %i1, 10
  br i1 %isubtmp, label %then, label %else

loop_exit:                                        ; preds = %then
  %i4 = load i32, i32* %i, align 4
  %calltmp = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([7 x i8], [7 x i8]* @str.3, i32 0, i32 0), i32 %i4)
  ret i32 0

then:                                             ; preds = %loop_body
  br label %loop_exit

else:                                             ; preds = %loop_body
  %i2 = load i32, i32* %i, align 4
  %iaddtmp = add i32 %i2, 1
  store i32 %iaddtmp, i32* %i, align 4
  %i3 = load i32, i32* %i, align 4
  br label %end

end:                                              ; preds = %else
  br label %loop_body
}
