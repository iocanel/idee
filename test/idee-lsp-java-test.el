;; 
;; Unless required by applicable law or agreed to in writing, software
;; distributed under the License is distributed on an "AS IS" BASIS,
;; WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
;; See the License for the specific language governing permissions and
;; limitations under the License.
;; 


;; Author: Ioannis Canellos

;; Version: 0.0.1

;; Package-Requires: ((emacs "25.1"))

;;; Commentary:

;;; Code:

(require 'test-helper)
(require 'idee-lsp-java (f-expand "idee-lsp-java.el" root-code-path))

(ert-deftest idee-java-lsp-project-root/maven-test ()
  "Should recognize maven project."
  (with-sandbox
   (f-mkdir ".git")
   (f-touch "pom.xml")
   (should (idee-lsp-java-project-p (f-full root-sandbox-path)))))

(ert-deftest idee-java-lsp-project-root/gradle-test ()
  "Should recognize gradle project."
  (with-sandbox
   (f-mkdir ".git")
   (f-touch "build.gradle")
   (should (idee-lsp-java-project-p (f-full root-sandbox-path)))))

(ert-deftest idee-java-lsp-project-root/eclipse-test ()
  "Should recognize eclipse project."
  (with-sandbox
   (f-mkdir ".git")
   (f-touch ".project")
   (should (idee-lsp-java-project-p (f-full root-sandbox-path)))))

(ert-deftest idee-java-lsp-project-root/other-test ()
  "Should recognize other project."
  (with-sandbox
   (f-mkdir ".git")
   (should (not (idee-lsp-java-project-p (f-full root-sandbox-path))))))
(provide 'idee-projects-test)
;;; idee-projects-test.el ends here
