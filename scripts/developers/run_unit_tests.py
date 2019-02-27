#!/usr/bin/env python

from __future__ import print_function
import os.path
import tempfile
import subprocess
import shutil
import argparse
import timeit

script_root = os.path.abspath(os.path.dirname(__file__))
root = os.path.join(script_root, '../..')
allowed_hosts = os.listdir(os.path.join(root, 'src/drivers'))

parser = argparse.ArgumentParser()
parser.add_argument('--host', action='append', dest='hosts', choices=allowed_hosts)
parser.add_argument('--cmake', default='cmake')
parser.add_argument('--compiler')
parser.add_argument('-p', '--performance', action='store_true')
parser.add_argument('--config', default='fabm.yaml')
parser.add_argument('--env', default='environment.yaml')
parser.add_argument('--report', default=None)
parser.add_argument('--repeat', type=int, default=5)
args = parser.parse_args()
if args.performance:
    assert os.path.isfile(args.config)
    assert os.path.isfile(args.env)
    if args.report is None:
        git_branch = subprocess.check_output(['git', 'name-rev', '--name-only', 'HEAD']).decode('ascii').strip()
        git_commit = subprocess.check_output(['git', 'describe', '--always', '--dirty']).decode('ascii').strip()
        args.report = 'performance_%s_%s.log' % (git_branch, git_commit)
        print('Performance report will be written to %s' % args.report)

cmake_arguments = []
if args.compiler is not None:
    cmake_arguments.append('-DCMAKE_Fortran_COMPILER=%s' % args.compiler)

generates = {}
builds = {}
tests = {}

if not args.hosts:
    args.hosts = allowed_hosts
print('Selected hosts: %s' % ', '.join(args.hosts))

logs = []
def run(phase, args, **kwargs):
    proc = subprocess.Popen(args, stdout=subprocess.PIPE, stderr=subprocess.STDOUT, **kwargs)
    stdoutdata, stderrdata = proc.communicate()
    if proc.returncode != 0:
        log_path = '%s.log' % phase
        with open(log_path, 'wb') as f:
            f.write(stdoutdata)
        logs.append(log_path)
        print('FAILED (log written to %s)' % log_path)
    else:
        print('SUCCESS')
    return proc.returncode

build_root = tempfile.mkdtemp()
try:
    vsconfig = 'Release' if args.performance else 'Debug'
    host2exe = {}
    for host in args.hosts:
        print(host)
        build_dir = os.path.join(build_root, host)
        os.mkdir(build_dir)
        print('  generating...', end='')
        generates[host] = run('%s_generate' % host, [args.cmake, os.path.join(root, 'src'), '-DFABM_HOST=%s' % host] + cmake_arguments, cwd=build_dir)
        if generates[host] != 0:
            continue
        print('  building...', end='')
        builds[host] = run('%s_build' % host, [args.cmake, '--build', build_dir, '--target', 'test_host', '--config', vsconfig])
        if builds[host] != 0:
            continue
        print('  testing...', end='')
        for exename in ('%s/test_host.exe' % vsconfig, 'test_host'):
            exepath = os.path.join(build_dir, exename)
            if os.path.isfile(exepath):
                host2exe[host] = exepath
        tests[host] = run('%s_test' % host, [host2exe[host]])

    if args.performance:
        print('Measuring runtime')
        timings = {}
        shutil.copy(args.config, os.path.join(build_root, 'fabm.yaml'))
        shutil.copy(args.env, os.path.join(build_root, 'environment.yaml'))
        for i in range(args.repeat):
            print('  replicate %i' % i)
            for host in args.hosts:
                if tests.get(host, 1) != 0:
                    continue
                start = timeit.default_timer()
                print('    %s...' % (host,), end='')
                run('%s_perfrun_%i' % (host, i), [host2exe[host], '--simulate'], cwd=build_root)
                timings.setdefault(host, []).append(timeit.default_timer() - start)

finally:
    shutil.rmtree(build_root)

if logs:
    print('All tests complete - %i FAILED' % len(logs))
    print('See the following log files:\n%s' % '\n'.join(logs))
else:
    print('All tests complete - no failures')

if args.performance:
    print('Timings:')
    for host in args.hosts:
        ts = timings.get(host, ())
        timing = 'NA' if not ts else '%.3f s' % (sum(ts) / len(ts))
        print('  %s: %s' % (host, timing))
    with open(args.report, 'w') as f:
        f.write('host\t%s\taverage (s)\n' % '\t'.join(['run %i (s)' % i for i in range(args.repeat)]))
        for host in args.hosts:
            if host in timings:
                ts = timings[host]
                f.write('%s\t%s\t%.3f\n' % (host, '\t'.join(['%.3f' % t for t in ts]), sum(ts) / len(ts)))