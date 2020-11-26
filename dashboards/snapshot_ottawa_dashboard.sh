
#!/bin/sh
CHROME="/Applications/Google Chrome.app/Contents/MacOS/Google Chrome"
DATE=`date +"%Y-%m-%d"`
BASEURL="https://app.powerbi.com/view?r=eyJrIjoiMTBhZTU4YmUtNGI1MC00ZGZjLTgzOGYtNGFiZjdiNmIzNDk3IiwidCI6ImRmY2MwMzNkLWRmODctNGM2ZS1hMWI4LThlYWE3M2YxYjcyZSJ9"
"$CHROME" --headless --disable-gpu --virtual-time-budget=5000 \
    --print-to-pdf=dashboard_${DATE}_page1.pdf \
    "$BASEURL&pageName=ReportSectiond3db0329f5a43a351b64"
"$CHROME" --headless --disable-gpu --virtual-time-budget=10000 \
    --print-to-pdf=dashboard_${DATE}_page2.pdf \
    "$BASEURL&pageName=ReportSection589411c28cc80d80cc14"
"$CHROME" --headless --disable-gpu --virtual-time-budget=5000 \
    --print-to-pdf=dashboard_${DATE}_page3.pdf \
    "$BASEURL&pageName=ReportSection3f159c9ca4691f023509"
"$CHROME" --headless --disable-gpu --virtual-time-budget=10000 \
    --print-to-pdf=dashboard_${DATE}_page4.pdf \
    "$BASEURL&pageName=ReportSectionc4838ab7bc44487dadbf"
"$CHROME" --headless --disable-gpu --virtual-time-budget=10000 \
    --print-to-pdf=dashboard_${DATE}_page5.pdf \
    "$BASEURL&pageName=ReportSectionf29e3e3cd2143ed056b2"
