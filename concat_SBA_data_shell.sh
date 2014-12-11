# xlsx2csv.py [-h] [-v] [-a] [-d DELIMITER] [-f DATEFORMAT] [-i] [-e]
#               [-p SHEETDELIMITER] [-s SHEETID] [--hyperlinks]
#               [-I INCLUDE_SHEET_PATTERN] [-E EXCLUDE_SHEET_PATTERN]
#               xlsxfile [outfile]


# from xlsx2csv import *
# xlsx2csv("dir\ID_01.xlsx", open("ID_01.csv", "w+"))


xlsx2csv/xlsx2csv.py -s 4 SBA_individual_loan_data/7a_504_DataFile_1990_201402.xlsx sheet4.csv



# cat lix_header_engage_nosupers.txt > lix_alldata.txt;
# for i in `ls joined_lix/`; do
# cat joined_lix/$i>> lix_alldata.txt;
# done


cat individual_files/2013_SBA_Loans_Full_20140915.csv  | head -1 > header_dash_tmp.txt ;

