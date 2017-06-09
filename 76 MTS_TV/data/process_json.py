#!/usr/bin/python
# -*- coding: cp1251 -*-
import os
import time

def main():
    
    src_name = "stat.json"
    # src_name = "head.json"
    

    print(os.path.basename(os.getcwd()))
    print(os.getcwd())
    fin = open(src_name, 'r')
    num_lines = 0
    # num_lines = sum(1 for line in fin) # посчитали общее число строк в файле
    print(num_lines)
    fin.close()

    start = time.time()    
    try:
        fin = open(src_name, 'r')
        fout = open("stat_fix.json", 'w')
        i = 0
        
        fout.write('[\n')
        for line in fin:
            # print(value='', sep=' ', end='\n', file=sys.stdout, flush=False)
            # print(line)
            i = i + 1
            # print(i)
            fout.write(line.replace('\n', ',') + '\n')
            
            if i % 10000 == 0: 
                print(i)
    finally:
        fout.write(']')        
        fin.close()
        fout.close()
    
    end = time.time()   
    print(end - start, "s")


if __name__ == '__main__':
    main()
    


        
        
        