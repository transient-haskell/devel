pkill hsWebServer
rm -r data/*
cp DefaultConstitution.hs data
./hsWebServer 80
