; ModuleID = 'main'
source_filename = "main"

@str = private unnamed_addr constant [4 x i8] c"%d\0A\00", align 1

declare i32 @printf(i8*, ...)

declare i32* @malloc(i64)

define i32 @main() {
entry:
  %a = alloca i32, align 4
  store i32 1, i32* %a, align 4
  %b = alloca i32, align 4
  store i32 1, i32* %b, align 4
  %c = alloca i32, align 4
  store i32 2, i32* %c, align 4
  %d = alloca i32, align 4
  store i32 3, i32* %d, align 4
  %x = alloca i32, align 4
  %a1 = load i32, i32* %a, align 4
  %isubtmp = icmp eq i32 %a1, 1
  br i1 %isubtmp, label %then, label %else

then:                                             ; preds = %entry
  %b2 = load i32, i32* %b, align 4
  %a3 = load i32, i32* %a, align 4
  %isubtmp4 = icmp eq i32 %b2, %a3
  br i1 %isubtmp4, label %then5, label %else6

else:                                             ; preds = %entry
  br label %end

end:                                              ; preds = %end7, %else
  %if_result8 = phi i32 [ %if_result, %end7 ], [ 2, %else ]
  store i32 %if_result8, i32* %x, align 4
  %x9 = load i32, i32* %x, align 4
  %calltmp = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([4 x i8], [4 x i8]* @str, i32 0, i32 0), i32 %x9)
  %x10 = load i32, i32* %x, align 4
  ret i32 %x10

then5:                                            ; preds = %then
  br label %end7

else6:                                            ; preds = %then
  br label %end7

end7:                                             ; preds = %else6, %then5
  %if_result = phi i32 [ 3, %then5 ], [ 1, %else6 ]
  br label %end
}
