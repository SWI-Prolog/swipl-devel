#! /bin/sh

XEMU=$1
options=$2

echo "-------------------------------------------------------"
echo "---   Running ptq/test.sh                           ---"
echo "-------------------------------------------------------"

#ensure parser.P is compiled
#---------------------------
$XEMU $options << EOF
[parser].
halt.
EOF

    # XEMU and options must be together in quotes
./genlredscaff.sh "tran([john,talks,about,a,unicorn])" jtaulredemu "$XEMU $options"
./diffall.sh jtaulredemu jtaulredint
./genscaff.sh "tran([mary,believes_that,john,finds,a,unicorn,and,he,eats,it])" mbjfauaheiemu  "$XEMU $options"
./diffall.sh mbjfauaheiemu mbjfauaheiint
./genlredscaff.sh "tran([mary,believes_that,john,finds,a,unicorn,and,he,eats,it])" mbjfauaheilredemu  "$XEMU $options"
./diffall.sh mbjfauaheilredemu mbjfauaheilredint
echo "************************************************************"
echo "End Short"
./genscaff.sh "tran([john,talks,about,a,unicorn])" jtauemu "$XEMU $options"
./diffall.sh jtauemu jtauint
./genscaff.sh "tran([john,dates,mary])" jdmemu "$XEMU $options"
./diffall.sh jdmemu jdmint
./genscaff.sh "tran([a,price,rises])" apremu "$XEMU $options"
./diffall.sh apremu aprint
./genscaff.sh "tran([john,dates,him])" jdhemu "$XEMU $options"
./diffall.sh jdhemu jdhint
#./genscaff.sh "tran([john,walks,in,a,park])" jwiapemu "$XEMU $options"
#./diffall.sh jwiapemu jwiapint
#./genscaff.sh "tran([john,finds,a,unicorn])" jfauemu "$XEMU $options"
#./diffall.sh jfauemu jfauint
./genscaff.sh "tran([john,seeks,a,unicorn])" jsauemu "$XEMU $options"
./diffall.sh jsauemu jsauint
#./genscaff.sh "tran([a,man,tries_to,walk])" amtwemu "$XEMU $options"
#./diffall.sh amtwemu amtwint
./genscaff.sh "tran([a,woman,tries_to,run,allegedly])" awtraemu "$XEMU $options"
./diffall.sh awtraemu awtraint
./genscaff.sh "tran([every,man,loves,a,woman])" emlawemu "$XEMU $options"
./diffall.sh emlawemu emlawint
./genscaff.sh "tran([a,woman,such_that,she,walks,runs])" awsswremu "$XEMU $options"
./diffall.sh awsswremu awsswrint
./genscaff.sh "tran([john,seeks,a,unicorn,and,mary,seeks,it])" jsauamsiemu "$XEMU $options"
./diffall.sh jsauamsiemu jsauamsiint
echo "************************************************************"
echo "Testing Lreds"
./genlredscaff.sh "tran([john,dates,mary])" jdmlredemu "$XEMU $options"
./diffall.sh jdmlredemu jdmlredint
#./genlredscaff.sh "tran([a,price,rises])" aprlredemu "$XEMU $options"
#./diffall.sh aprlredemu aprlredint
./genlredscaff.sh "tran([john,dates,him])" jdhlredemu "$XEMU $options"
./diffall.sh jdhlredemu jdhlredint
./genlredscaff.sh "tran([john,walks,in,a,park])" jwiaplredemu "$XEMU $options"
./diffall.sh jwiaplredemu jwiaplredint
./genlredscaff.sh "tran([john,finds,a,unicorn])" jfaulredemu "$XEMU $options"
./diffall.sh jfaulredemu jfaulredint
./genlredscaff.sh "tran([john,seeks,a,unicorn])" jsaulredemu "$XEMU $options"
./diffall.sh jsaulredemu jsaulredint
#./genlredscaff.sh "tran([a,man,tries_to,walk])" amtwlredemu "$XEMU $options"
#./diffall.sh amtwlredemu amtwlredint
./genlredscaff.sh "tran([a,woman,tries_to,run,allegedly])" awtralredemu "$XEMU $options"
./diffall.sh awtralredemu awtralredint
./genlredscaff.sh "tran([every,man,loves,a,woman])" emlawlredemu "$XEMU $options"
./diffall.sh emlawlredemu emlawlredint
./genlredscaff.sh "tran([a,woman,such_that,she,walks,runs])" awsswrlredemu "$XEMU $options"
./diffall.sh awsswrlredemu awsswrlredint
#./genlredscaff.sh "tran([john,seeks,a,unicorn,and,mary,seeks,it])" jsauamsiemu "$XEMU $options"
#./diffall.sh jsauamsiemu jsauamsilredint
#./genlredscaff.sh "tran([john,believes_that,mary,walks,in,a,park])" jbmwiaplredemu "$XEMU $options"
#./diffall.sh jbmwiaplredemu jbmwiaplredint
./genlredscaff.sh "tran([john,believes_that,mary,wishes_to,walk,in,a,park])" jbmwwaiplredemu "$XEMU $options"
./diffall.sh jbmwwaiplredemu jbmwwaiplredint
./genlredscaff.sh "tran([john,wishes_to,find,a,unicorn,and,eat,it])" jwfauaeilredemu "$XEMU $options"
./diffall.sh jwfauaeilredemu jwfauaeilredint
./genlredscaff.sh "tran([john,tries_to,find,a,unicorn,and,wishes_to,eat,it])" jtfauaweilredemu "$XEMU $options"
./diffall.sh jtfauaweilredemu jtfauaweilredint
./genlredscaff.sh "tran([every,man,loves,a,woman,such_that,she,loves,him])" emlawsslhlredemu "$XEMU $options"
./diffall.sh emlawsslhlredemu emlawsslhlredint
./genlredscaff.sh "tran([every,man,loves,a,woman,and,she,loves,him])" emlawaslhlredemu "$XEMU $options"
./diffall.sh emlawaslhlredemu emlawaslhlredint

#./genlredscaff.sh "tran([every,man,loves,a,woman,in,a,park,voluntarily,and,every,fish,eats,a,pen,slowly])" emlawiapvaefeapslredemu
#./diffall.sh emlawiapvaefeapslredemu emlawiapvaefeapslredint

