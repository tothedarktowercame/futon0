import unittest, sys, os, importlib
sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
mod = importlib.import_module('crosstab_stage_intent')

if __name__ == '__main__':
    suite = unittest.defaultTestLoader.loadTestsFromTestCase(mod.TestCrosstab)
    unittest.TextTestRunner(verbosity=2).run(suite)
