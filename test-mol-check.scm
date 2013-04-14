#!/bin/sh
:;if which gosh > /dev/null 2>&1; then
:;   exec gosh -- "$0" "$@"
:;else
:;   exec ~/apl/gauche/bin/gosh -- "$0" "$@"
:;fi

(use gauche.test)
(use www.cgi.test)

(test-start "mol-check Test")

(test* "mol-check"
       "<html><head><title>table</title\n><style type=\"text/css\">span.notok {background-color : #ffcccc;}</style\n></head\n><body><pre>使用できるコメントアウト ; # ... <= >> ~ </pre\n><pre>使用できる原子+-HDHeTLiBeBCNOFNeNaMgAlSiPSClArKCaScTiVCrMnFeCoNiCuZnGaGeAsSeBrKrRbSrYZrNbMoTcRuRhPdAgCdInSnSbTeIXeCsBaLaHfTaWReOsIrPtAuHgTlPbBiPoAtRnFrRaAcRfDbSgBhHsMtUunUuuUubUutCePrNdPmSmEuGdTbDyHoErTmYbLuAcThPaUNpPuAmCmBkCfEsFmMdNoLr</pre\n><p>分子式を記入し、'check'ボタンを押して下さい。</p\n><form><p><textarea rows=\"8\" cols=\"100\" name=\"p\"></textarea\n></p\n><p><input type=\"submit\" name=\"submit\" value=\"check\" /></p\n><pre>  0 ok   Ar                   <= (Ar)                                              </pre\n></form\n></body\n></html\n>"
       (values-ref (run-cgi-script->string
                    "./mol-check.cgi"
                    :environment '((REQUEST_METHOD . "GET"))
                    :parameters '((p . "Ar")))
                   1))
