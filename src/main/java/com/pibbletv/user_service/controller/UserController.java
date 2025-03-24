package com.pibbletv.user_service.controller;

import com.pibbletv.user_service.business.interfaces.UserService;
import lombok.AllArgsConstructor;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

@RestController
@RequestMapping("/category")
@AllArgsConstructor
public class UserController {

    private final UserService userService;
}
