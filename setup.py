# coding: utf-8
from setuptools import setup, find_packages
import programslice


setup(name=programslice.__name__,
      version=programslice.__version__,
      description='Static analysis tool for python',
      long_description=(
          open('README.rst').read() + '\n\n' +
          open('docs/CHANGES.rst').read()
      ),
      keywords='static analysis',
      classifiers=[
          'Environment :: Console',
          'Intended Audience :: Developers',
          'License :: OSI Approved :: GNU General Public License (GPL)',
          'Programming Language :: Python',
          'Programming Language :: Python :: 2.7',
          'Topic :: Software Development',
      ],
      author=u'Róman Joost',
      author_email='roman@bromeco.de',
      url='https://github.com/romanofski/programslice',
      license='GPLv3',
      packages=find_packages(exclude=['ez_setup', 'examples', 'tests']),
      include_package_data=True,
      zip_safe=False,
      install_requires=[],
      extras_require=dict(
          test=[]
          ),
      entry_points={
          'console_scripts': [
              'programslice = programslice:command_slice_file',
          ]
      }
      )
