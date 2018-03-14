#!/usr/bin/python
# -*- coding: cp1251 -*-

# import string, sys, os.path
# import re
import time
import win32com.client
from win32com.client import constants

def main():
 # xlOpenXMLWorkbookMacroEnabled = 52
 
 xlApp = win32com.client.Dispatch("Excel.Application")
 xlApp.Visible = True
 xlApp.AskToUpdateLinks = False
 # https://answers.microsoft.com/en-us/msoffice/forum/msoffice_excel-msoffice_custom-mso_2007/open-excel-file-with-vba-with-macros-disabled/615d2426-e59e-42bc-83ae-c82f51bb805f?auth=1 
 xlApp.EnableEvents = False
 xlApp.DisplayAlerts = True
 # xlwb = xlApp.Workbooks.Open(r'D:\Temp\1\IT.P-18-99 ДРП-6.1 (для импорта) v3-unprotected.xlsm')
 # xlwb = xlApp.Workbooks.Open(r'D:\Temp\1\IT.P-18-99 ДРП-6.1 (для импорта) v3.xlsm')
 xlwb = xlApp.Workbooks.Open(r'D:\Temp\1\11.xlsx', UpdateLinks=3)
 # xlwb.SaveAs("D:\\Temp\\1\\res.xls", FileFormat = 56, ConflictResolution = "xlLocalSessionChanges") # http://www.rondebruin.nl/win/s5/win001.htm
 time.sleep(5)
 print("Ready to save")
 # xlwb.SaveAs("res.xlsm", FileFormat = "xlOpenXMLWorkbookMacroEnabled", ConflictResolution = "xlLocalSessionChanges")
 xlwb.SaveAs("res.xlsx", FileFormat = "xlOpenXMLWorkbook", ConflictResolution = "xlLocalSessionChanges")
 sheet = xlwb.Worksheets('Работы - НН-ОЦО')  #.Select()
 print(sheet.Range("C139").Value)

 # xlwb.RunAutoMacros("xlAutoClose") # не запускается
 print(xlwb.ActiveSheet.Name)
 xlwb.Worksheets('Для загрузки').Activate()
 print(xlwb.ActiveSheet.Name)
# xlwb.Save()
 # xlApp.EnableEvents = True
 print("Ready to save")
 # xlwb.SaveAs("res.xlsm", FileFormat = "xlOpenXMLWorkbookMacroEnabled", ConflictResolution = "xlLocalSessionChanges")
 xlwb.SaveAs("res.xlsx", FileFormat = "xlOpenXMLWorkbook", ConflictResolution = "xlLocalSessionChanges")
 # xlwb.SaveAs("res.xlsm")
 print("Ready to close")
 xlwb.Close(SaveChanges=1)
    
if __name__ == '__main__':
 print(constants)
 main()
