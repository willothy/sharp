; ModuleID = 'main'
source_filename = "main"

%node = type { i32, %node* }

@str = private unnamed_addr constant [10 x i8] c"head: %d\0A\00", align 1
@str.1 = private unnamed_addr constant [15 x i8] c"head.next: %d\0A\00", align 1
@str.2 = private unnamed_addr constant [20 x i8] c"head.next.next: %d\0A\00", align 1

declare i32 @printf(i8*, ...)

declare i8* @malloc(i64)

declare void @free(i8*)

define %node* @new_node(i32 %0) {
entry:
  %ptr = alloca %node*, align 8
  %malloc_call = call i8* @malloc(i64 ptrtoint (%node* getelementptr (%node, %node* null, i32 1) to i64))
  %ptr_cast = bitcast i8* %malloc_call to %node*
  store %node* %ptr_cast, %node** %ptr, align 8
  %ptr1 = load %node*, %node** %ptr, align 8
  %node.value_LHS = getelementptr inbounds %node, %node* %ptr1, i32 0, i32 0
  store i32 %0, i32* %node.value_LHS, align 4
  %deref_assign_result = load i32, i32* %node.value_LHS, align 4
  %ptr2 = load %node*, %node** %ptr, align 8
  ret %node* %ptr2
}

define %node* @push_front(%node* %0, i32 %1) {
entry:
  %ptr = alloca %node*, align 8
  %new_node_call = call %node* @new_node(i32 %1)
  store %node* %new_node_call, %node** %ptr, align 8
  %ptr1 = load %node*, %node** %ptr, align 8
  %node.next_LHS = getelementptr inbounds %node, %node* %ptr1, i32 0, i32 1
  store %node* %0, %node** %node.next_LHS, align 8
  %deref_assign_result = load %node*, %node** %node.next_LHS, align 8
  %ptr2 = load %node*, %node** %ptr, align 8
  ret %node* %ptr2
}

define void @node_test() {
entry:
  %head = alloca %node*, align 8
  %new_node_call = call %node* @new_node(i32 1)
  store %node* %new_node_call, %node** %head, align 8
  %a = alloca i32, align 4
  store i32 2, i32* %a, align 4
  %b = alloca i32, align 4
  store i32 3, i32* %b, align 4
  %head1 = load %node*, %node** %head, align 8
  %a2 = load i32, i32* %a, align 4
  %push_front_call = call %node* @push_front(%node* %head1, i32 %a2)
  store %node* %push_front_call, %node** %head, align 8
  %head3 = load %node*, %node** %head, align 8
  %head4 = load %node*, %node** %head, align 8
  %b5 = load i32, i32* %b, align 4
  %push_front_call6 = call %node* @push_front(%node* %head4, i32 %b5)
  store %node* %push_front_call6, %node** %head, align 8
  %head7 = load %node*, %node** %head, align 8
  %head8 = load %node*, %node** %head, align 8
  %node.value = getelementptr inbounds %node, %node* %head8, i32 0, i32 0
  %"*i32_deref" = load i32, i32* %node.value, align 4
  %printf_call = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([10 x i8], [10 x i8]* @str, i32 0, i32 0), i32 %"*i32_deref")
  %head9 = load %node*, %node** %head, align 8
  %node.next = getelementptr inbounds %node, %node* %head9, i32 0, i32 1
  %"**node_deref" = load %node*, %node** %node.next, align 8
  %node.value10 = getelementptr inbounds %node, %node* %"**node_deref", i32 0, i32 0
  %"*i32_deref11" = load i32, i32* %node.value10, align 4
  %printf_call12 = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([15 x i8], [15 x i8]* @str.1, i32 0, i32 0), i32 %"*i32_deref11")
  %head13 = load %node*, %node** %head, align 8
  %node.next14 = getelementptr inbounds %node, %node* %head13, i32 0, i32 1
  %"**node_deref15" = load %node*, %node** %node.next14, align 8
  %node.next16 = getelementptr inbounds %node, %node* %"**node_deref15", i32 0, i32 1
  %"**node_deref17" = load %node*, %node** %node.next16, align 8
  %node.value18 = getelementptr inbounds %node, %node* %"**node_deref17", i32 0, i32 0
  %"*i32_deref19" = load i32, i32* %node.value18, align 4
  %printf_call20 = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([20 x i8], [20 x i8]* @str.2, i32 0, i32 0), i32 %"*i32_deref19")
  %head21 = load %node*, %node** %head, align 8
  %"*node_deref" = load %node, %node* %head21, align 8
  %next = extractvalue %node %"*node_deref", 1
  %"*node_deref22" = load %node, %node* %next, align 8
  %next23 = extractvalue %node %"*node_deref22", 1
  %ptr_cast = bitcast %node* %next23 to i8*
  call void @free(i8* %ptr_cast)
  %head24 = load %node*, %node** %head, align 8
  %"*node_deref25" = load %node, %node* %head24, align 8
  %next26 = extractvalue %node %"*node_deref25", 1
  %ptr_cast27 = bitcast %node* %next26 to i8*
  call void @free(i8* %ptr_cast27)
  %head28 = load %node*, %node** %head, align 8
  %ptr_cast29 = bitcast %node* %head28 to i8*
  call void @free(i8* %ptr_cast29)
  ret void
}

define i32 @main() {
entry:
  call void @node_test()
  ret i32 0
}
