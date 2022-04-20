import sys, os
from subprocess import call, check_output, Popen, PIPE

path = '.'
action = 'compile'

def file_exists(file_path):
    if not file_path:
        return False
    else:
        return os.path.isfile(file_path)

def main():
  for root, dirs, files in os.walk(path):
    print('Checking ' + root)
    makefile = os.path.join(root, "Makefile")
    if file_exists(makefile):
      cmd = 'cd ' + root + '; make ' + action
      #cmd = 'ls -la'
      pipes = Popen(cmd, shell=True, stdout=PIPE, stderr=PIPE)
      std_out, std_err = pipes.communicate()
      
      if (action == 'compile') | (action == 'run'):
        if pipes.returncode != 0:
          # an error happened!
          err_msg = "%s. Code: %s" % (std_err.strip(), pipes.returncode)
          print('[E] Error on ' + root + ': ')
          print(err_msg)
        elif len(std_err):
          # return code is 0 (no error), but we may want to
          # do something with the info on std_err
          # i.e. logger.warning(std_err)
          print('[OK]')
        else:
          print('[OK]')
      if action == 'measure':
        call(['sleep', '5'])

if __name__ == '__main__':
  if len(sys.argv) == 2:
    act = sys.argv[1]
    if (act == 'compile') | (act == 'run') | (act == 'clean') | (act == 'measure'):
      print('Performing \"' + act + '\" action...')
      action = act
    else:
      print('Error: Unrecognized action \"' + act + '\"')
      sys.exit(1)
  else:
    print('Performing \"compile\" action...')
    action = 'compile'
  
  main()
    
