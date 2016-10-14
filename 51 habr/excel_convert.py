#!/usr/bin/python
# -*- coding: cp1251 -*-

# import string, sys, os.path
# import re
import win32com.client
from win32com.client import constants

def main():
 xlOpenXMLWorkbookMacroEnabled = 52
 
 xlApp = win32com.client.Dispatch("Excel.Application")
 xlwb = xlApp.Workbooks.Open("D:\\Temp\\alcall_details_bristol_w31_1.xls")

 xlwb.SaveAs("D:\\Temp\\alcall_details_bristol_w31_3.xls", FileFormat = 56) # http://www.rondebruin.nl/win/s5/win001.htm
 xlwb.Close(SaveChanges=1)
    
if __name__ == '__main__':
 print constants
 main()
