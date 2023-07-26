;; 
;; Unless required by applicable law or agreed to in writing, software
;; distributed under the License is distributed on an "AS IS" BASIS,
;; WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
;; See the License for the specific language governing permissions and
;; limitations under the License.
;; 


;; Author: Ioannis Canellos

;; Version: 0.0.1

;; Package-Requires: ((emacs "28.0"))

;;; Commentary:

;;; Code:

(require 'test-helper)

(ert-deftest idee/javascript-project-root/npm-test ()
  "Should recognize npm project."
  (with-sandbox
   (f-mkdir ".git")
   (f-touch "package.json")
   (should (idee/javascript-project-p (f-full root-sandbox-path)))))

(ert-deftest idee/javascript-project-root/other-test ()
  "Should recognize other project."
  (with-sandbox
   (f-mkdir ".git")
   (should (not (idee/javascript-project-p (f-full root-sandbox-path))))))
(provide 'idee-projects-test)
;;; idee-projects-test.el ends here
