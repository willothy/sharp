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
  %push_front_call = call %node* @push_front(%node* %head1, i32 2)
  store %node* %push_front_call, %node** %head, align 8
  %head2 = load %node*, %node** %head, align 8
  %head3 = load %node*, %node** %head, align 8
  %push_front_call4 = call %node* @push_front(%node* %head3, i32 3)
  store %node* %push_front_call4, %node** %head, align 8
  %head5 = load %node*, %node** %head, align 8
  %head6 = load %node*, %node** %head, align 8
  %node.value = getelementptr inbounds %node, %node* %head6, i32 0, i32 0
  %"*i32_deref" = load i32, i32* %node.value, align 4
  %printf_call = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([10 x i8], [10 x i8]* @str, i32 0, i32 0), i32 %"*i32_deref")
  %head7 = load %node*, %node** %head, align 8
  %node.next = getelementptr inbounds %node, %node* %head7, i32 0, i32 1
  %"**node_deref" = load %node*, %node** %node.next, align 8
  %node.value8 = getelementptr inbounds %node, %node* %"**node_deref", i32 0, i32 0
  %"*i32_deref9" = load i32, i32* %node.value8, align 4
  %printf_call10 = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([15 x i8], [15 x i8]* @str.1, i32 0, i32 0), i32 %"*i32_deref9")
  %head11 = load %node*, %node** %head, align 8
  %node.next12 = getelementptr inbounds %node, %node* %head11, i32 0, i32 1
  %"**node_deref13" = load %node*, %node** %node.next12, align 8
  %node.next14 = getelementptr inbounds %node, %node* %"**node_deref13", i32 0, i32 1
  %"**node_deref15" = load %node*, %node** %node.next14, align 8
  %node.value16 = getelementptr inbounds %node, %node* %"**node_deref15", i32 0, i32 0
  %"*i32_deref17" = load i32, i32* %node.value16, align 4
  %printf_call18 = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([20 x i8], [20 x i8]* @str.2, i32 0, i32 0), i32 %"*i32_deref17")
  %head19 = load %node*, %node** %head, align 8
  %"*node_deref" = load %node, %node* %head19, align 8
  %next = extractvalue %node %"*node_deref", 1
  %"*node_deref20" = load %node, %node* %next, align 8
  %next21 = extractvalue %node %"*node_deref20", 1
  %ptr_cast = bitcast %node* %next21 to i8*
  call void @free(i8* %ptr_cast)
  %head22 = load %node*, %node** %head, align 8
  %"*node_deref23" = load %node, %node* %head22, align 8
  %next24 = extractvalue %node %"*node_deref23", 1
  %ptr_cast25 = bitcast %node* %next24 to i8*
  call void @free(i8* %ptr_cast25)
  %head26 = load %node*, %node** %head, align 8
  %ptr_cast27 = bitcast %node* %head26 to i8*
  call void @free(i8* %ptr_cast27)
  ret void
}

define i32 @main() {
entry:
  call void @node_test()
  ret i32 0
}
