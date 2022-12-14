; ModuleID = 'main'
source_filename = "main"

@str = private unnamed_addr constant [4 x i8] c"%d\0A\00", align 1

declare i32 @printf(i8*, ...)

declare i32* @malloc(i64)

define i32 @main() {
entry:
  %a = alloca i32, align 4
  br i1 false, label %then, label %else

then:                                             ; preds = %entry
  br label %end

else:                                             ; preds = %entry
  br label %end

end:                                              ; preds = %else, %then
  %if_result = phi i32 [ 5, %then ], [ 2, %else ]
  store i32 %if_result, i32* %a, align 4
  %a1 = load i32, i32* %a, align 4
  %calltmp = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([4 x i8], [4 x i8]* @str, i32 0, i32 0), i32 %a1)
  %a2 = load i32, i32* %a, align 4
  ret i32 %a2
}
