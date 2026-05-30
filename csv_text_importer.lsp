;;; ==========================================================================
;;; [CSV_TEXT_IMPORTER] 엑셀/CSV 좌표 기반 텍스트 삽입 도구
;;; 지원 환경: AutoCAD Map 3D 2000 이상 (ANSI 환경 최적화)
;;; 
;;; [주의사항]
;;; 1. CSV 파일 저장 시 'CSV (쉼표로 분리)' 형식을 사용하세요.
;;; 2. 메모장에서 열었을 때 인코딩이 ANSI(EUC-KR)여야 한글이 깨지지 않습니다.
;;; 3. CSV 데이터 구조: X좌표, Y좌표, Z좌표, 텍스트내용 (번호 없음)
;;; ==========================================================================

(vl-load-com)

;;; [도우미] 특정 문자의 개수를 세는 함수
(defun csv:count-char (str charcode / count)
  (setq count 0)
  (foreach c (vl-string->list str)
    (if (= c charcode) (setq count (1+ count)))
  )
  count
)

;;; [도우미] 엑셀 특유의 따옴표 및 줄바꿈 처리된 CSV 파싱 함수
(defun csv:parse-quoted-line (fn / line next-line)
  (setq line (read-line fn))
  (if line
    (progn
      ;; 따옴표 개수가 홀수라면 줄바꿈이 포함된 것이므로 짝수가 될 때까지 다음 줄을 읽음
      (while (and (= (rem (csv:count-char line 34) 2) 1)
                  (setq next-line (read-line fn)))
        (setq line (strcat line "\n" next-line))
      )
    )
  )
  line
)

;;; [도우미] 따옴표를 고려하여 필드 분리
(defun csv:split-quoted-string (str / fields current in-quote i c)
  (setq fields '() current "" in-quote nil i 1)
  (repeat (strlen str)
    (setq c (substr str i 1))
    (cond
      ((= c "\"") (setq in-quote (not in-quote))) ; 따옴표 상태 반전
      ((and (= c ",") (not in-quote)) ; 따옴표 밖의 쉼표라면 필드 구분
       (setq fields (cons current fields) current ""))
      (t (setq current (strcat current c)))
    )
    (setq i (1+ i))
  )
  (reverse (cons current fields))
)

;;; [도우미] 문자열 정제 (앞뒤 따옴표/공백 제거, 내부 쌍따옴표 및 줄바꿈 처리)
(defun csv:clean-text (str sep / tmp result pos part i c)
  (setq tmp "" i 1)
  ;; 1. 모든 공백, 탭, 큰따옴표를 한 글자씩 검사하며 원천 제거
  (repeat (strlen str)
    (setq c (substr str i 1))
    (if (not (member c '(" " "\t" "\"")))
      (setq tmp (strcat tmp c))
    )
    (setq i (1+ i))
  )
  ;; 2. 줄바꿈(\n)을 기준으로 분리하며, 내용이 없는(비어있는) 줄은 무시
  (setq result "")
  (while (setq pos (vl-string-search "\n" tmp))
    (setq part (substr tmp 1 pos)) ; \n 이전까지
    (setq tmp (substr tmp (+ pos 2))) ; \n 이후부터
    (if (/= part "") ; 줄에 내용이 있는 경우에만 결과에 추가
      (setq result (if (= result "") part (strcat result sep part)))
    )
  )
  ;; 3. 마지막 남은 문자열 처리
  (if (/= tmp "")
    (setq result (if (= result "") tmp (strcat result sep tmp)))
  )
  result
)

;;; 메인 명령어: CTI (CSV Text Import)
(defun c:CTI (/ f fn line data-list x-coord y-coord z-coord txt-val pt count old-os out-mode sep sel-style sel-height st style-names skip)
  (princ "\n[CTI] CSV 좌표 기반 텍스트 삽입을 시작합니다.")

  ;; 0. 출력 형식 선택 (MTEXT vs DTEXT)
  (initget "Mtext Dtext")
  (setq out-mode (getkword "\n출력 형식을 선택하세요 [Mtext(줄바꿈)/Dtext(한줄)] <Dtext>: "))
  (if (null out-mode) (setq out-mode "Dtext"))
  (if (= out-mode "Dtext") (princ "\n[안내] Dtext 선택 시 줄바꿈 문자는 공백 없이 '//'로 대체됩니다."))

   ;; 0.1 스타일 선택 (도면 내 스타일 목록 추출)
  (setq style-list '())
  (setq st (tblnext "STYLE" t))
  (while st
    (setq style-list (cons (cdr (assoc 2 st)) style-list))
    (setq st (tblnext "STYLE"))
  )
  (setq style-list (reverse style-list))

  ;; 우선순위 스타일 필터링 (ghs, ngsw, standard)
  (setq priority-list '())
  (foreach p '("ghs" "ngsw" "standard")
    (if (tblsearch "STYLE" p)
      (setq priority-list (append priority-list (list p)))
    )
  )

  ;; 우선순위 스타일이 있으면 해당 리스트만 사용, 없으면 전체 리스트 사용
  (if (> (length priority-list) 0)
    (setq target-list priority-list)
    (setq target-list style-list)
  )

  ;; 번호와 함께 스타일 리스트 출력
  (princ "\n--- 사용할 스타일 번호를 선택하세요 ---")
  (setq i 1)
  (foreach s target-list
    (princ (strcat "\n [" (itoa i) "] " s))
    (setq i (1+ i))
  )

  (setq choice (getstring (strcat "\n선택할 스타일 번호 입력 <1 (" (car target-list) ")>: ")))
  (if (or (= choice "") (not (distof choice)))
    (setq sel-style (car target-list))
    (progn
      (setq idx (1- (fix (distof choice))))
      (if (and (>= idx 0) (< idx (length target-list)))
        (setq sel-style (nth idx target-list))
        (setq sel-style (car target-list))
      )
    )
  )
  (princ (strcat "\n[확인] '" sel-style "' 스타일이 적용됩니다."))

  ;; 0.2 높이 선택
  (setq sel-height (getdist (strcat "\n텍스트 높이를 입력하세요 <0.3>: ")))
  (if (null sel-height) (setq sel-height 0.3)) 

  ;; 1. 파일 선택
  (setq f (getfiled "좌표 CSV 파일 선택 (ANSI 인코딩 필수)" "" "csv" 0))
  
  (if (and f (setq fn (open f "r")))
    (progn
      (setq count 0)
      (setq old-os (getvar "osmode"))
      (setvar "osmode" 0) ; 오스냅 끄기
      
      ;; 첫 줄(헤더) 건너뛰기 여부 확인
      (initget "Yes No")
      (setq skip (getkword "\n첫 번째 줄이 제목입니까? [예(Yes)/아니오(No)] <Yes>: "))
      (if (or (null skip) (= skip "Yes")) (read-line fn))

      (princ "\n데이터를 처리 중입니다...")

      ;; 2. 한 줄씩 읽기
      (while (setq line (csv:parse-quoted-line fn))
        (if (/= line "")
          (progn
            ;; 따옴표를 고려하여 데이터 분리
            (setq data-list (csv:split-quoted-string line))
            
            ;; 데이터 구조 파싱 (X, Y, Z, 텍스트)
            ;; 인덱스는 0부터 시작
            (if (>= (length data-list) 3)
              (progn
                (setq x-coord (distof (nth 0 data-list)))
                (setq y-coord (distof (nth 1 data-list)))
                (setq z-coord (distof (nth 2 data-list)))
                
                ;; 좌표가 유효한 숫자인 경우에만 진행
                (if (and x-coord y-coord z-coord)
                  (progn
                    ;; 텍스트 정제 (선택한 모드에 따라 줄바꿈 처리 다름)
                    (setq sep (if (= out-mode "Mtext") "\\P" "//"))
                    (setq txt-val (if (nth 3 data-list) (csv:clean-text (nth 3 data-list) sep) ""))
                    (setq pt (list x-coord y-coord z-coord))

                    ;; 3. 텍스트 생성
                    (if (= out-mode "Mtext")
                      (entmake ; MTEXT 생성
                        (list 
                          '(0 . "MTEXT")
                          '(100 . "AcDbEntity")
                          '(100 . "AcDbMText")
                          (cons 8 "0")
                          (cons 10 pt)
                          (cons 40 sel-height)
                          (cons 1 txt-val)
                          (cons 7 sel-style)
                          (cons 71 1) ; Attachment point: TopLeft
                        )
                      )
                      (entmake ; DTEXT(TEXT) 생성
                        (list 
                          '(0 . "TEXT")
                          '(100 . "AcDbEntity")
                          '(100 . "AcDbText")
                          (cons 8 "0")
                          (cons 10 pt)
                          (cons 40 sel-height)
                          (cons 1 txt-val)
                          (cons 7 sel-style)
                          (cons 50 0.0)
                        )
                      )
                    )
                    (setq count (1+ count))
                  )
                )
              )
            )
          )
        )
      )
      (close fn)
      (setvar "osmode" old-os)
      (princ (strcat "\n[완료] 총 " (itoa count) "개의 텍스트를 삽입하였습니다."))
    )
    (princ "\n[취소] 파일을 열 수 없거나 선택되지 않았습니다.")
  )
  (princ)
)

;;; 안내 메시지
(princ "\nCSV_TEXT_IMPORTER 로드됨. 실행 명령어: CTI")
(princ "\n주의: CSV 파일은 반드시 ANSI 인코딩으로 저장되어야 한글이 인식됩니다.")
(princ)