; ModuleID = 'main'
source_filename = "main"

%node = type { i32, %node* }

@str = private unnamed_addr constant [13 x i8] c"ptr val: %d\0A\00", align 1

declare i32 @printf(i8*, ...)

declare %node* @malloc(i64)

declare void @free(%node*)

define i32 @main() {
entry:
  %ptr = alloca %node*, align 8
  %calltmp = call %node* @malloc(i64 ptrtoint (%node* getelementptr (%node, %node* null, i32 1) to i64))
  store %node* %calltmp, %node** %ptr, align 8
  %ptr1 = load %node*, %node** %ptr, align 8
  %node.value = getelementptr inbounds %node, %node* %ptr1, i32 0, i32 0
  store i32 1, i32* %node.value, align 4
  %deref_assign_result = load i32, i32* %node.value, align 4
  %ptr2 = load %node*, %node** %ptr, align 8
  %value = getelementptr inbounds %node, %node* %ptr2, i32 0, i32 0
  %deref = load i32, i32* %value, align 4
  %calltmp3 = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([13 x i8], [13 x i8]* @str, i32 0, i32 0), i32 %deref)
  %ptr4 = load %node*, %node** %ptr, align 8
  call void @free(%node* %ptr4)
  ret i32 0
}
