;; -*- coding: utf-8; lexical-binding: t -*-

(clear-abbrev-table global-abbrev-table)

(define-abbrev-table 'global-abbrev-table
  '(
     ;; ISO-8859-1 characters
     ("htmlAgrave" "À")
     ("htmlAacute" "Á")
     ("htmlAcirc" "Â")
     ("htmlAtilde" "Ã")
     ("htmlAuml" "Ä")
     ("htmlAring" "Å")
     ("htmlAElig" "Æ")
     ("htmlCcedil" "Ç")
     ("htmlEgrave" "È")
     ("htmlEacute" "É")
     ("htmlEcirc" "Ê")
     ("htmlEuml" "Ë")
     ("htmlIgrave" "Ì")
     ("htmlIacute" "Í")
     ("htmlIcirc" "Î")
     ("htmlIuml" "Ï")
     ("htmlETH" "Ð")
     ("htmlNtilde" "Ñ")
     ("htmlOgrave" "Ò")
     ("htmlOacute" "Ó")
     ("htmlOcirc" "Ô")
     ("htmlOtilde" "Õ")
     ("htmlOuml" "Ö")
     ("htmlOslash" "Ø")
     ("htmlUgrave" "Ù")
     ("htmlUacute" "Ú")
     ("htmlUcirc" "Û")
     ("htmlUuml" "Ü")
     ("htmlYacute" "Ý")
     ("htmlTHORN" "Þ")
     ("htmlszlig" "ß")
     ("htmlagrave" "à")
     ("htmlaacute" "á")
     ("htmlacirc" "â")
     ("htmlatilde" "ã")
     ("htmlauml" "ä")
     ("htmlaring" "å")
     ("htmlaelig" "æ")
     ("htmlccedil" "ç")
     ("htmlegrave" "è")
     ("htmleacute" "é")
     ("htmlecirc" "ê")
     ("htmleuml" "ë")
     ("htmligrave" "ì")
     ("htmliacute" "í")
     ("htmlicirc" "î")
     ("htmliuml" "ï")
     ("htmleth" "ð")
     ("htmlntilde" "ñ")
     ("htmlograve" "ò")
     ("htmloacute" "ó")
     ("htmlocirc" "ô")
     ("htmlotilde" "õ")
     ("htmlouml" "ö")
     ("htmloslash" "ø")
     ("htmlugrave" "ù")
     ("htmluacute" "ú")
     ("htmlucirc" "û")
     ("htmluuml" "ü")
     ("htmlyacute" "ý")
     ("htmlthorn" "þ")
     ("htmlyuml" "ÿ")
     ;; ISO-8859-1 symbols
     ("htmliexcl" "¡")
     ("htmlcent" "¢")
     ("htmlpound" "£")
     ("htmlcurren" "¤")
     ("htmlyen" "¥")
     ("htmlbrvbar" "¦")
     ("htmlsect" "§")
     ("htmluml" "¨")
     ("htmlcopy" "©")
     ("htmlordf" "ª")
     ("htmllaquo" "«")
     ("htmlnot" "¬")
     ("­", "htmlshy")
     ("htmlreg" "®")
     ("htmlmacr" "¯")
     ("htmldeg" "°")
     ("htmlplusmn" "±")
     ("htmlsup2" "²")
     ("htmlsup3" "³")
     ("htmlacute" "´")
     ("htmlmicro" "µ")
     ("htmlpara" "¶")
     ("htmlcedil" "¸")
     ("htmlsup1" "¹")
     ("htmlordm" "º")
     ("htmlraquo" "»")
     ("htmlfrac14" "¼")
     ("htmlfrac12" "½")
     ("htmlfrac34" "¾")
     ("htmliquest" "¿")
     ("htmltimes" "×")
     ("htmldivide" "÷")
     ;; math symbols
     ("htmlforall" "∀")
     ("htmlpart" "∂")
     ("htmlexist" "∃")
     ("htmlempty" "∅")
     ("htmlnabla" "∇")
     ("htmlisin" "∈")
     ("htmlnotin" "∉")
     ("htmlni" "∋")
     ("htmlprod" "∏")
     ("htmlsum" "∑")
     ("htmlminus" "−")
     ("htmllowast" "∗")
     ("htmlradic" "√")
     ("htmlprop" "∝")
     ("htmlinfin" "∞")
     ("htmlang" "∠")
     ("htmland" "∧")
     ("htmlor" "∨")
     ("htmlcap" "∩")
     ("htmlcup" "∪")
     ("htmlint" "∫")
     ("htmlthere4" "∴")
     ("htmlsim" "∼")
     ("htmlcong" "≅")
     ("htmlasymp" "≈")
     ("htmlne" "≠")
     ("htmlequiv" "≡")
     ("htmlle" "≤")
     ("htmlge" "≥")
     ("htmlsub" "⊂")
     ("htmlsup" "⊃")
     ("htmlnsub" "⊄")
     ("htmlsube" "⊆")
     ("htmlsupe" "⊇")
     ("htmloplus" "⊕")
     ("htmlotimes" "⊗")
     ("htmlperp" "⊥")
     ("htmlsdot" "⋅")
     ;; Greek letters
     ("htmlAlpha" "Α")
     ("htmlBeta" "Β")
     ("htmlGamma" "Γ")
     ("htmlDelta" "Δ")
     ("htmlEpsilon" "Ε")
     ("htmlZeta" "Ζ")
     ("htmlEta" "Η")
     ("htmlTheta" "Θ")
     ("htmlIota" "Ι")
     ("htmlKappa" "Κ")
     ("htmlLambda" "Λ")
     ("htmlMu" "Μ")
     ("htmlNu" "Ν")
     ("htmlXi" "Ξ")
     ("htmlOmicron" "Ο")
     ("htmlPi" "Π")
     ("htmlRho" "Ρ")
     ("htmlSigma" "Σ")
     ("htmlTau" "Τ")
     ("htmlUpsilon" "Υ")
     ("htmlPhi" "Φ")
     ("htmlChi" "Χ")
     ("htmlPsi" "Ψ")
     ("htmlOmega" "Ω")
     ("htmlalpha" "α")
     ("htmlbeta" "β")
     ("htmlgamma" "γ")
     ("htmldelta" "δ")
     ("htmlepsilon" "ε")
     ("htmlzeta" "ζ")
     ("htmleta" "η")
     ("htmltheta" "θ")
     ("htmliota" "ι")
     ("htmlkappa" "κ")
     ("htmllambda" "λ")
     ("htmlmu" "μ")
     ("htmlnu" "ν")
     ("htmlxi" "ξ")
     ("htmlomicron" "ο")
     ("htmlpi" "π")
     ("htmlrho" "ρ")
     ("htmlsigmaf" "ς")
     ("htmlsigma" "σ")
     ("htmltau" "τ")
     ("htmlupsilon" "υ")
     ("htmlphi" "φ")
     ("htmlchi" "χ")
     ("htmlpsi" "ψ")
     ("htmlomega" "ω")
     ("htmlthetasym" "ϑ")
     ("htmlupsih" "ϒ")
     ("htmlpiv" "ϖ")
     ;; misc
     ("htmlOElig" "Œ")
     ("htmloelig" "œ")
     ("htmlScaron" "Š")
     ("htmlscaron" "š")
     ("htmlYuml" "Ÿ")
     ("htmlfnof" "ƒ")
     ("htmlcirc" "ˆ")
     ("htmltilde" "˜")
     ("htmlndash" "–")
     ("htmlmdash" "—")
     ("htmllsquo" "‘")
     ("htmlrsquo" "’")
     ("htmlsbquo" "‚")
     ("htmlldquo" "“")
     ("htmlrdquo" "”")
     ("htmlbdquo" "„")
     ("htmldagger" "†")
     ("htmlDagger" "‡")
     ("htmlbull" "•")
     ("htmlhellip" "…")
     ("htmlpermil" "‰")
     ("htmlprime" "′")
     ("htmlPrime" "″")
     ("htmllsaquo" "‹")
     ("htmlrsaquo" "›")
     ("htmloline" "‾")
     ("htmleuro" "€")
     ("htmltrade" "™")
     ("htmllarr" "←")
     ("htmluarr" "↑")
     ("htmlrarr" "→")
     ("htmldarr" "↓")
     ("htmlharr" "↔")
     ("htmlcrarr" "↵")
     ("htmllceil" "⌈")
     ("htmlrceil" "⌉")
     ("htmllfloor" "⌊")
     ("htmlrfloor" "⌋")
     ("htmlloz" "◊")
     ("htmlspades" "♠")
     ("htmlclubs" "♣")
     ("htmlhearts" "♥")
     ("htmldiams" "♦")
     ))

(set-default 'abbrev-mode t)

;; disable prompt for saving abbrevs (still doesn't work when reinstalling packages, but at least
;; I tried...)
;; (setq save-abbrevs 'silently)
(setq save-abbrevs nil)
