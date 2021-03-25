
#!/bin/sh
CHROME="/Applications/Google Chrome.app/Contents/MacOS/Google Chrome"
DATE=`date +"%Y-%m-%d"`
BASEURL="https://app.powerbi.com/view?r=eyJrIjoiNmQ3YzRiZGEtYTAyNC00YjgyLWEwNGMtMjQ4YWJjY2JmZGVlIiwidCI6ImRmY2MwMzNkLWRmODctNGM2ZS1hMWI4LThlYWE3M2YxYjcyZSJ9"
"$CHROME" --headless --disable-gpu --virtual-time-budget=5000 \
    --print-to-pdf=dashboard_${DATE}_page1.pdf \
    "$BASEURL"
"$CHROME" --headless --disable-gpu --virtual-time-budget=10000 \
    --print-to-pdf=dashboard_${DATE}_page2.pdf \
    "$BASEURL&pageName=ReportSection4d8d09f90ed9cc5513d5"
"$CHROME" --headless --disable-gpu --virtual-time-budget=5000 \
    --print-to-pdf=dashboard_${DATE}_page3.pdf \
    "$BASEURL&pageName=ReportSectionf2da438ea5d04a89daeb"
"$CHROME" --headless --disable-gpu --virtual-time-budget=10000 \
    --print-to-pdf=dashboard_${DATE}_page4.pdf \
    "$BASEURL&pageName=ReportSection70be3fe934dac4a89802"
"$CHROME" --headless --disable-gpu --virtual-time-budget=10000 \
    --print-to-pdf=dashboard_${DATE}_page5.pdf \
    "$BASEURL&pageName=ReportSection0e9cac292303a6058854"
