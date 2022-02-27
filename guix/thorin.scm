(use-modules (gnu))

(use-modules (nongnu packages linux)
             (nongnu system linux-initrd))

(use-service-modules cups
                     desktop
                     networking
                     pm
                     security-token
                     ssh
                     xorg)

(use-package-modules emacs
                     fonts
                     terminals
                     version-control
                     wm)

(define %thorin-packages
  (append
   (list
    (specification->package "brightnessctl")
    (specification->package "emacs")
    (specification->package "font-dejavu")
    (specification->package "font-fira-code")
    (specification->package "font-google-material-design-icons")
    (specification->package "font-google-noto")
    (specification->package "gcc-toolchain")
    (specification->package "gdb")
    (specification->package "git")
    (specification->package+output "git" "send-email")
    (specification->package "nss-certs")
    (specification->package "nyxt")
    (specification->package "openssh")
    (specification->package "sbcl")
    (specification->package "sbcl-slynk")
    (specification->package "sbcl-stumpwm-cpu")
    (specification->package "sbcl-stumpwm-net")
    (specification->package "sbcl-stumpwm-pass")
    (specification->package "sbcl-stumpwm-ttf-fonts")
    (specification->package "sbcl-stumpwm-wifi")
    (specification->package "st")
    (specification->package "stumpwm-with-slynk")
    (specification->package+output "stumpwm" "lib"))
   %base-packages))

(define %thorin-xorg-libinput-config
  "Section \"InputClass\"
     Identifier		\"Touchpad\"
     MatchIsTouchpad	\"on\"
     Driver		\"libinput\"
     Option		\"NaturalScrolling\" \"1\"
   EndSection")

(define %thorin-services
  (append
   (list (service connman-service-type)
         (service pcscd-service-type)
         (service tlp-service-type
                  (tlp-configuration
                   (cpu-scaling-governor-on-ac (list "performance"))
                   (cpu-scaling-governor-on-bat (list "powersave"))
                   (ahci-runtime-pm-on-ac? #t)
                   (ahci-runtime-pm-on-bat? #t)
                   (sched-powersave-on-bat? #t))))
   (modify-services
    %desktop-services
    (delete network-manager-service-type)
    (guix-service-type config =>
                       (guix-configuration
                        (inherit config)
                        (substitute-urls
                         (append (list "https://substitutes.nonguix.org")
                                 %default-substitute-urls))
                        (authorized-keys
                         (append (list
                                  (plain-file
                                   "non-guix.pub"
                                   "(public-key
                                     (ecc
                                      (curve Ed25519)
                                      (q #C1FD53E5D4CE971933EC50C9F307AE2171A2D3B52C804642A7A35F84F3A4EA98#)))"))
                                 %default-authorized-guix-keys))))
    (gdm-service-type config =>
                 (gdm-configuration
                  (xorg-configuration
                   (xorg-configuration
                    (extra-config (list %thorin-xorg-libinput-config)))))))))

(operating-system
 (kernel linux)
 (initrd microcode-initrd)
 (firmware (list linux-firmware))
 (locale "en_US.utf8")
 (timezone "Europe/Helsinki")
 (keyboard-layout (keyboard-layout "us"))
 (host-name "thorin")
 (users (cons* (user-account
                (name "balbi")
                (comment "Felipe Balbi")
                (group "users")
                (home-directory "/home/balbi")
                (supplementary-groups
                 '("wheel" "netdev" "audio" "video" "dialout")))
               %base-user-accounts))

 (packages %thorin-packages)

 (services %thorin-services)

 (bootloader
  (bootloader-configuration
   (bootloader grub-bootloader)
   (targets (list "/dev/sda"))
   (keyboard-layout keyboard-layout)))

 (swap-devices
  (list (swap-space
         (target
          (uuid "636bac8e-8d9e-4896-afb9-99b45ca2d9d1")))))

 (file-systems
  (cons* (file-system
          (mount-point "/")
          (device
           (uuid "fc58cf5f-2235-4812-bd61-0aedb5067a54"
                 'ext4))
          (type "ext4"))
         %base-file-systems)))
