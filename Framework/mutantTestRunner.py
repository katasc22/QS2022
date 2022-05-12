import os, shutil, glob, subprocess, re
from distutils.dir_util import copy_tree

mutant_files = [f'./Mutants/mutantsJars/mbMutant{i}.jar' for i in range(1,6)]

output_intro = '<h2>Standard output</h2>\n<span class="code">\n<pre>! '

print('Copying the current model to mutation directory.')
# TODO change path to file found in framework
shutil.copy('./src/test/scala/at/tugraz/ist/qs2022/MessageBoardSpecification.scala', './Mutants/src/test/scala/at/tugraz/ist/qs2022/')

os.makedirs('./Mutants/currentMutant', exist_ok=True)

for index, mutant_jar in enumerate(mutant_files):
    print(f'Running tests on Mutant {index + 1}:')
    # delete previous mutant
    mutant_folder_content = glob.glob('./Mutants/currentMutant/*')
    for f in mutant_folder_content:
        os.remove(f)
    shutil.copy(mutant_jar, './Mutants/currentMutant/')

    gradle_command = './Mutants/gradlew' if os.name != 'nt' else '.\Mutants\gradlew.bat'

    process = subprocess.call([gradle_command, 'clean', 'build', '-p', './Mutants/'],stdout=subprocess.PIPE, stderr=subprocess.PIPE,)

    copy_tree('./Mutants/build/reports/tests/', f'./Mutants/reports/mutant{index + 1}')

    output = ''
    with open('./Mutants/build/reports/tests/test/classes/at.tugraz.ist.qs2022.MessageBoardSpecificationTest.html', 'r') as file:
        output = file.read()

    if output.find(output_intro) != -1:
        output = output[output.find(output_intro) + len(output_intro):]
        output = output[:output.find('</pre>')]
        print(output)
    else:
        print('Mutant conforms to the model.')

mutant_folder_content = glob.glob('./Mutants/currentMutant/*')
for f in mutant_folder_content:
    os.remove(f)

print('All experiments executed. Reports for each mutant are saved in "Mutants/reports/" directory.')


