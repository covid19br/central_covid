#!/usr/bin/awk -f
#143 campos
# 42 OUTRO_DES
# 57 MORB_DESC
# 133 OBSERVA
BEGIN {
    FS=";"
    ORS=""
}
{
    if ($1 ~ /^"[0-9]{12}"/ || NR==1){
        # inicio do registro
        # registro aberto?
        if (R == 1)
            print ";;;;;;;;;;\n"
        R=0
        for (i=1; i<42; i++){
            print $i";"
        }
        # campo OUTRO_DES
        pula_od=0
        while (! $(42+pula_od) ~ /"$/)
            pula_od++
        for (i=43+pula_od; i<57+pula_od; i++){
            print $i";"
        }

        # campo MORB_DESC
        pula_mb=pula_od
        while (! $(57+pula_mb) ~ /"$/)
            pula_mb++
        for (i=58+pula_mb; i<133+pula_mb; i++){
            print $i";"
        }
        if (NF >= 143+pula_mb && ($(NF-10) ~ /"$/ || NR == 1)){
            for(i=9; i>=0; i--){
                print $(NF-i)";"
            }
            print "\n"
        } else {
            R = 1
        }
    }
    else if (NF >= 11){
        # fim do registro continuado em outra linha
        for (i=9; i>=0; i--){
            print $(NF-i)";"
        }
        print "\n"
        R = 0
    }
}
